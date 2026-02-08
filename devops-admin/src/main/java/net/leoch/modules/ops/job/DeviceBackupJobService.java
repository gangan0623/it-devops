package net.leoch.modules.ops.job;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import net.leoch.common.utils.JsonUtils;
import net.leoch.modules.ops.dao.BackupAgentDao;
import net.leoch.modules.ops.dao.DeviceBackupDao;
import net.leoch.modules.ops.dto.BackupCallbackItem;
import net.leoch.modules.ops.entity.BackupAgentEntity;
import net.leoch.modules.ops.entity.DeviceBackupEntity;
import net.leoch.modules.ops.service.DeviceBackupHistoryService;
import net.leoch.modules.ops.service.DeviceBackupRecordService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 设备备份定时任务
 */
@Slf4j
@Service("deviceBackupJobService")
public class DeviceBackupJobService {
    private final DeviceBackupDao deviceBackupDao;
    private final BackupAgentDao backupAgentDao;
    private final DeviceBackupRecordService deviceBackupRecordService;
    private final DeviceBackupHistoryService deviceBackupHistoryService;

    public DeviceBackupJobService(DeviceBackupDao deviceBackupDao, BackupAgentDao backupAgentDao, DeviceBackupRecordService deviceBackupRecordService, DeviceBackupHistoryService deviceBackupHistoryService) {
        this.deviceBackupDao = deviceBackupDao;
        this.backupAgentDao = backupAgentDao;
        this.deviceBackupRecordService = deviceBackupRecordService;
        this.deviceBackupHistoryService = deviceBackupHistoryService;
    }

    public void backup(String params) {
        List<BackupAgentEntity> agents = backupAgentDao.selectList(
            new LambdaQueryWrapper<BackupAgentEntity>().eq(BackupAgentEntity::getStatus, 1)
        );
        if (CollUtil.isEmpty(agents)) {
            return;
        }
        for (BackupAgentEntity agent : agents) {
            if (agent == null || agent.getId() == null) {
                continue;
            }
            List<DeviceBackupEntity> devices = deviceBackupDao.selectList(
                new LambdaQueryWrapper<DeviceBackupEntity>()
                    .eq(DeviceBackupEntity::getAgentId, agent.getId())
                    .eq(DeviceBackupEntity::getStatus, 1)
            );
            if (CollUtil.isEmpty(devices)) {
                continue;
            }
            List<Map<String, Object>> requestList = buildRequest(devices);
            triggerAgentBackup(agent, requestList);
        }
    }

    public boolean handleCallback(String token, List<BackupCallbackItem> items) {
        if (StrUtil.isBlank(token) || CollUtil.isEmpty(items)) {
            return false;
        }
        log.info("[设备备份] 备份回调收到：token={}***，数量={}", token != null && token.length() > 4 ? token.substring(0, 4) : "****", items == null ? 0 : items.size());
        BackupAgentEntity agent = backupAgentDao.selectOne(
            new LambdaQueryWrapper<BackupAgentEntity>().eq(BackupAgentEntity::getToken, token).last("limit 1")
        );
        if (agent == null || agent.getId() == null) {
            return false;
        }
        List<DeviceBackupEntity> devices = deviceBackupDao.selectList(
            new LambdaQueryWrapper<DeviceBackupEntity>().eq(DeviceBackupEntity::getAgentId, agent.getId()).eq(DeviceBackupEntity::getStatus, 1)
        );
        Map<String, DeviceBackupEntity> deviceMap = devices == null ? Collections.emptyMap()
            : devices.stream()
                .filter(item -> item != null && StrUtil.isNotBlank(item.getInstance()))
                .collect(Collectors.toMap(DeviceBackupEntity::getInstance, item -> item, (a, b) -> a));
        for (BackupCallbackItem item : items) {
            if (item == null || StrUtil.isBlank(item.getInstance())) {
                continue;
            }
            DeviceBackupEntity device = deviceMap.get(item.getInstance());
            String name = device == null ? item.getInstance() : device.getName();
            String url = item.getUrl() == null ? "" : item.getUrl();
            boolean success = StrUtil.isNotBlank(url);
            deviceBackupRecordService.upsertRecord(name, item.getInstance(), url, success);
            deviceBackupHistoryService.saveHistory(name, item.getInstance(), url, success ? 1 : 0);
        }
        return true;
    }

    private List<Map<String, Object>> buildRequest(List<DeviceBackupEntity> devices) {
        List<Map<String, Object>> list = new ArrayList<>();
        for (DeviceBackupEntity device : devices) {
            if (device == null || StrUtil.isBlank(device.getInstance())) {
                continue;
            }
            Map<String, Object> item = new HashMap<>();
            item.put("instance", device.getInstance());
            item.put("name", device.getName());
            item.put("username", device.getUsername());
            item.put("password", device.getPassword());
            item.put("model", device.getDeviceModel());
            list.add(item);
        }
        return list;
    }

    private void triggerAgentBackup(BackupAgentEntity agent, List<Map<String, Object>> payload) {
        if (agent == null || CollUtil.isEmpty(payload)) {
            return;
        }
        String url = buildBackupUrl(agent.getInstance());
        if (StrUtil.isBlank(url)) {
            return;
        }
        HttpURLConnection connection = null;
        try {
            log.info("[设备备份] 触发备份节点接口（回调模式）：{}", url);
            connection = (HttpURLConnection) new URL(url).openConnection();
            connection.setRequestMethod("POST");
            connection.setConnectTimeout(5000);
            // 回调模式不关心同步响应
            connection.setReadTimeout(1000);
            connection.setDoOutput(true);
            connection.setRequestProperty("Content-Type", "application/json");
            if (StrUtil.isNotBlank(agent.getToken())) {
                connection.setRequestProperty("agent-token", agent.getToken());
            }
            String body = JsonUtils.toJsonString(payload);
            try (OutputStream out = connection.getOutputStream()) {
                out.write(body.getBytes(StandardCharsets.UTF_8));
            }
            int code = connection.getResponseCode();
            log.info("[设备备份] 节点{}({}) 触发响应code={}，等待回调", agent.getName(), agent.getInstance(), code);
        } catch (Exception ignore) {
            log.warn("[设备备份] 节点接口调用失败：{}", url, ignore);
        } finally {
            if (connection != null) {
                connection.disconnect();
            }
        }
    }

    private String buildBackupUrl(String instance) {
        if (StrUtil.isBlank(instance)) {
            return null;
        }
        String trimmed = instance.trim();
        try {
            String candidate = trimmed;
            if (!candidate.startsWith("http://") && !candidate.startsWith("https://")) {
                candidate = "http://" + candidate;
            }
            URI uri = new URI(candidate);
            String scheme = uri.getScheme() == null ? "http" : uri.getScheme();
            String host = uri.getHost();
            int port = uri.getPort();
            String path = uri.getPath();
            if (host == null && candidate.startsWith("http://")) {
                String raw = candidate.substring("http://".length());
                int slash = raw.indexOf('/');
                String hostPort = slash > -1 ? raw.substring(0, slash) : raw;
                String[] parts = hostPort.split(":");
                host = parts[0];
                if (parts.length > 1) {
                    try {
                        port = Integer.parseInt(parts[1]);
                    } catch (Exception ignore) {
                        port = -1;
                    }
                }
                path = slash > -1 ? raw.substring(slash) : "";
            }
            if (port == -1) {
                port = 8120;
            }
            if (path == null || path.isEmpty() || "/".equals(path)) {
                path = "/backup";
            } else if (!path.endsWith("/backup")) {
                if (path.endsWith("/")) {
                    path = path + "backup";
                } else {
                    path = path + "/backup";
                }
            }
            return new URI(scheme, null, host, port, path, null, null).toString();
        } catch (Exception ignore) {
            if (trimmed.startsWith("http://") || trimmed.startsWith("https://")) {
                return trimmed.endsWith("/backup") ? trimmed : trimmed + (trimmed.endsWith("/") ? "backup" : "/backup");
            }
            return "http://" + trimmed + ":8120/backup";
        }
    }
}
