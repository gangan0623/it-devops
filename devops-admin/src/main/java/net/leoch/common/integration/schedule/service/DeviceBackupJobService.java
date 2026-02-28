package net.leoch.common.integration.schedule.service;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import cn.hutool.json.JSONUtil;
import net.leoch.modules.ops.mapper.BackupAgentMapper;
import net.leoch.modules.ops.mapper.NetworkBackupDeviceMapper;
import net.leoch.modules.ops.vo.req.BackupCallbackItemReq;
import net.leoch.modules.ops.entity.BackupAgentEntity;
import net.leoch.modules.ops.entity.NetworkBackupDeviceEntity;
import net.leoch.modules.ops.service.IDeviceBackupHistoryService;
import net.leoch.modules.ops.service.IDeviceBackupRecordService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * 设备备份定时任务
 */
@Slf4j
@Service("deviceBackupJobService")
@RequiredArgsConstructor
public class DeviceBackupJobService {
    private final NetworkBackupDeviceMapper networkBackupDeviceMapper;
    private final BackupAgentMapper backupAgentMapper;
    private final IDeviceBackupRecordService deviceBackupRecordService;
    private final IDeviceBackupHistoryService deviceBackupHistoryService;

    public void backup(String params) {
        log.info("[设备备份] 开始执行设备备份任务, params={}", params);
        long startTime = System.currentTimeMillis();
        List<BackupAgentEntity> agents = backupAgentMapper.selectList(
            new LambdaQueryWrapper<BackupAgentEntity>().eq(BackupAgentEntity::getStatus, 1)
        );
        if (CollUtil.isEmpty(agents)) {
            log.warn("[设备备份] 无可用备份代理");
            return;
        }
        log.info("[设备备份] 找到{}个可用代理", agents.size());
        int totalDevices = 0;
        int successAgents = 0;
        for (BackupAgentEntity agent : agents) {
            if (agent == null || agent.getId() == null) {
                continue;
            }
            List<NetworkBackupDeviceEntity> networkDevices = networkBackupDeviceMapper.selectList(
                    new LambdaQueryWrapper<NetworkBackupDeviceEntity>()
                            .eq(NetworkBackupDeviceEntity::getAgentId, agent.getId())
                            .eq(NetworkBackupDeviceEntity::getStatus, 1)
                            .eq(NetworkBackupDeviceEntity::getBackupEnabled, 1)
            );
            if (CollUtil.isEmpty(networkDevices)) {
                log.debug("[设备备份] 代理{}({})无网络设备", agent.getName(), agent.getInstance());
                continue;
            }
            List<Map<String, Object>> networkRequestList = buildNetworkRequest(networkDevices);
            if (CollUtil.isNotEmpty(networkRequestList)) {
                totalDevices += networkRequestList.size();
                log.info("[设备备份] 代理{}({})网络设备数={}", agent.getName(), agent.getInstance(), networkRequestList.size());
                triggerAgentBackup(agent, networkRequestList);
                successAgents++;
            }
        }
        long elapsedTime = System.currentTimeMillis() - startTime;
        log.info("[设备备份] 任务执行完成, 触发代理数={}, 设备总数={}, 耗时={}ms", successAgents, totalDevices, elapsedTime);
    }

    public boolean handleCallback(String token, List<BackupCallbackItemReq> items) {
        if (StrUtil.isBlank(token) || CollUtil.isEmpty(items)) {
            log.warn("[设备备份] 回调参数无效, token={}, itemsSize={}", token, items != null ? items.size() : 0);
            return false;
        }
        log.info("[设备备份] 收到备份回调, token={}***, 数量={}", token.length() > 4 ? token.substring(0, 4) : "****", items.size());
        BackupAgentEntity agent = backupAgentMapper.selectOne(
            new LambdaQueryWrapper<BackupAgentEntity>().eq(BackupAgentEntity::getToken, token).last("limit 1")
        );
        if (agent == null || agent.getId() == null) {
            log.error("[设备备份] 回调token无效, 找不到对应代理, token={}", token);
            return false;
        }
        log.info("[设备备份] 找到回调代理: {}({})", agent.getName(), agent.getInstance());
        List<NetworkBackupDeviceEntity> networkDevices = networkBackupDeviceMapper.selectList(
                new LambdaQueryWrapper<NetworkBackupDeviceEntity>()
                        .eq(NetworkBackupDeviceEntity::getAgentId, agent.getId())
                        .eq(NetworkBackupDeviceEntity::getStatus, 1)
                        .eq(NetworkBackupDeviceEntity::getBackupEnabled, 1)
        );
        Map<String, NetworkBackupDeviceEntity> networkDeviceMap = new HashMap<>();
        if (networkDevices != null) {
            for (NetworkBackupDeviceEntity item : networkDevices) {
                if (item == null || StrUtil.isBlank(item.getInstance())) {
                    continue;
                }
                networkDeviceMap.putIfAbsent(item.getInstance(), item);
            }
        }
        int successCount = 0;
        int failCount = 0;
        Date now = new Date();
        for (BackupCallbackItemReq item : items) {
            if (item == null || StrUtil.isBlank(item.getInstance())) {
                continue;
            }
            NetworkBackupDeviceEntity networkDevice = networkDeviceMap.get(item.getInstance());
            String name = networkDevice == null ? item.getInstance() : networkDevice.getName();
            String url = item.getUrl() == null ? "" : item.getUrl();
            boolean success = StrUtil.isNotBlank(url);
            if (success) {
                successCount++;
                log.info("[设备备份] 设备备份成功, instance={}, name={}, url={}", item.getInstance(), name, url);
            } else {
                failCount++;
                log.warn("[设备备份] 设备备份失败, instance={}, name={}", item.getInstance(), name);
            }
            deviceBackupRecordService.upsertRecord(name, item.getInstance(), url, success);
            deviceBackupHistoryService.saveHistory(name, item.getInstance(), url, success ? 1 : 0);
            if (networkDevice != null) {
                networkBackupDeviceMapper.update(null, new LambdaUpdateWrapper<NetworkBackupDeviceEntity>()
                        .eq(NetworkBackupDeviceEntity::getId, networkDevice.getId())
                        .set(NetworkBackupDeviceEntity::getLastBackupTime, now)
                        .set(NetworkBackupDeviceEntity::getLastBackupStatus, success ? 1 : 0)
                        .set(NetworkBackupDeviceEntity::getLastBackupMessage, success ? "OK" : "备份失败"));
            }
        }
        log.info("[设备备份] 回调处理完成, 代理={}, 成功={}, 失败={}", agent.getName(), successCount, failCount);
        return true;
    }

    private List<Map<String, Object>> buildNetworkRequest(List<NetworkBackupDeviceEntity> networkDevices) {
        List<Map<String, Object>> list = new ArrayList<>();
        if (networkDevices != null) {
            for (NetworkBackupDeviceEntity device : networkDevices) {
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
        }
        return list;
    }

    private void triggerAgentBackup(BackupAgentEntity agent, List<Map<String, Object>> payload) {
        if (agent == null || CollUtil.isEmpty(payload)) {
            log.warn("[设备备份] 触发参数无效, agentId={}, agentName={}, payloadSize={}",
                    agent != null ? agent.getId() : null,
                    agent != null ? agent.getName() : null,
                    payload != null ? payload.size() : 0);
            return;
        }
        String url = buildBackupUrl(agent.getInstance());
        if (StrUtil.isBlank(url)) {
            log.error("[设备备份] 构建备份URL失败, instance={}", agent.getInstance());
            return;
        }
        HttpURLConnection connection = null;
        long startTime = System.currentTimeMillis();
        try {
            log.info("[设备备份] 触发备份代理, name={}, url={}, 设备数={}", agent.getName(), url, payload.size());
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
            String body = JSONUtil.toJsonStr(payload);
            try (OutputStream out = connection.getOutputStream()) {
                out.write(body.getBytes(StandardCharsets.UTF_8));
            }
            int code = connection.getResponseCode();
            long elapsedTime = System.currentTimeMillis() - startTime;
            log.info("[设备备份] 代理触发成功, name={}, code={}, 耗时={}ms, 等待回调", agent.getName(), code, elapsedTime);
        } catch (Exception e) {
            long elapsedTime = System.currentTimeMillis() - startTime;
            log.error("[设备备份] 代理触发失败, name={}, url={}, 耗时={}ms", agent.getName(), url, elapsedTime, e);
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
            String candidate = ensureScheme(trimmed);
            URI uri = new URI(candidate);
            String scheme = uri.getScheme() == null ? "http" : uri.getScheme();
            String host = uri.getHost();
            int port = uri.getPort();
            String path = uri.getPath();

            if (host == null) {
                UrlComponents components = parseHostPortPath(candidate);
                host = components.host;
                port = components.port;
                path = components.path;
            }

            port = normalizePort(port);
            path = normalizePath(path);

            return new URI(scheme, null, host, port, path, null, null).toString();
        } catch (Exception e) {
            log.warn("[设备备份] 构建URL失败, instance={}", instance, e);
            return buildFallbackUrl(trimmed);
        }
    }

    private String ensureScheme(String url) {
        if (!url.startsWith("http://") && !url.startsWith("https://")) {
            return "http://" + url;
        }
        return url;
    }

    private UrlComponents parseHostPortPath(String candidate) {
        String raw = candidate.startsWith("http://")
                ? candidate.substring("http://".length())
                : candidate;
        int slash = raw.indexOf('/');
        String hostPort = slash > -1 ? raw.substring(0, slash) : raw;
        String[] parts = hostPort.split(":");

        String host = parts[0];
        int port = -1;
        if (parts.length > 1) {
            try {
                port = Integer.parseInt(parts[1]);
            } catch (Exception e) {
                log.warn("[设备备份] 解析端口失败, portStr={}", parts[1], e);
            }
        }
        String path = slash > -1 ? raw.substring(slash) : "";

        return new UrlComponents(host, port, path);
    }

    private int normalizePort(int port) {
        return port == -1 ? 8120 : port;
    }

    private String normalizePath(String path) {
        if (path == null || path.isEmpty() || "/".equals(path)) {
            return "/backup";
        }
        if (!path.endsWith("/backup")) {
            return path.endsWith("/") ? path + "backup" : path + "/backup";
        }
        return path;
    }

    private String buildFallbackUrl(String trimmed) {
        if (trimmed.startsWith("http://") || trimmed.startsWith("https://")) {
            return trimmed.endsWith("/backup") ? trimmed : trimmed + (trimmed.endsWith("/") ? "backup" : "/backup");
        }
        return "http://" + trimmed + ":8120/backup";
    }

    private static class UrlComponents {
        final String host;
        final int port;
        final String path;

        UrlComponents(String host, int port, String path) {
            this.host = host;
            this.port = port;
            this.path = path;
        }
    }
}
