package net.leoch.modules.job.task;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import net.leoch.common.redis.RedisKeys;
import net.leoch.common.redis.RedisUtils;
import net.leoch.common.utils.PingUtils;
import net.leoch.modules.ops.mapper.BusinessSystemMapper;
import net.leoch.modules.ops.mapper.BackupAgentMapper;
import net.leoch.modules.ops.mapper.DeviceBackupMapper;
import net.leoch.modules.ops.mapper.LinuxHostMapper;
import net.leoch.modules.ops.mapper.WindowHostMapper;
import net.leoch.modules.ops.entity.BackupAgentEntity;
import net.leoch.modules.ops.entity.BusinessSystemEntity;
import net.leoch.modules.ops.entity.DeviceBackupEntity;
import net.leoch.modules.ops.entity.LinuxHostEntity;
import net.leoch.modules.ops.entity.WindowHostEntity;
import net.leoch.common.utils.MetricsUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.*;
import java.util.function.Function;

/**
 * 在线状态刷新定时任务
 *
 * onlineStatusRefreshTask 为 spring bean 名称
 */
@Component("onlineStatusRefreshTask")
public class OnlineStatusRefreshTask implements ITask {
    private static final Logger logger = LoggerFactory.getLogger(OnlineStatusRefreshTask.class);
    private static final long CACHE_EXPIRE_SECONDS = 600;

    private final LinuxHostMapper linuxHostMapper;
    private final WindowHostMapper windowHostMapper;
    private final BusinessSystemMapper businessSystemMapper;
    private final BackupAgentMapper backupAgentMapper;
    private final DeviceBackupMapper deviceBackupMapper;
    private final RedisUtils redisUtils;

    public OnlineStatusRefreshTask(LinuxHostMapper linuxHostMapper,
                                   WindowHostMapper windowHostMapper,
                                   BusinessSystemMapper businessSystemMapper,
                                   BackupAgentMapper backupAgentMapper,
                                   DeviceBackupMapper deviceBackupMapper,
                                   RedisUtils redisUtils) {
        this.linuxHostMapper = linuxHostMapper;
        this.windowHostMapper = windowHostMapper;
        this.businessSystemMapper = businessSystemMapper;
        this.backupAgentMapper = backupAgentMapper;
        this.deviceBackupMapper = deviceBackupMapper;
        this.redisUtils = redisUtils;
    }

    @Override
    public void run(String params) {
        long start = System.currentTimeMillis();
        refreshLinux();
        refreshWindows();
        refreshBusinessSystems();
        refreshBackupAgents();
        refreshDeviceBackups();
        logger.info("OnlineStatusRefreshTask finished in {} ms", System.currentTimeMillis() - start);
    }

    private void refreshLinux() {
        List<LinuxHostEntity> list = linuxHostMapper.selectList(new LambdaQueryWrapper<LinuxHostEntity>()
                .select(LinuxHostEntity::getInstance));
        Map<String, Object> statusMap = refreshWithThreads(list, LinuxHostEntity::getInstance,
                instance -> MetricsUtils.metricsOk(instance, 3000));
        refreshCache(RedisKeys.getLinuxHostOnlineKey(), statusMap);
    }

    private void refreshWindows() {
        List<WindowHostEntity> list = windowHostMapper.selectList(new LambdaQueryWrapper<WindowHostEntity>()
                .select(WindowHostEntity::getInstance));
        Map<String, Object> statusMap = refreshWithThreads(list, WindowHostEntity::getInstance,
                instance -> MetricsUtils.metricsOk(instance, 3000));
        refreshCache(RedisKeys.getWindowHostOnlineKey(), statusMap);
    }

    private void refreshBusinessSystems() {
        List<BusinessSystemEntity> list = businessSystemMapper.selectList(new LambdaQueryWrapper<BusinessSystemEntity>()
                .select(BusinessSystemEntity::getInstance));
        Map<String, Object> statusMap = refreshWithThreads(list, BusinessSystemEntity::getInstance,
                instance -> PingUtils.isReachable(instance, 2000));
        refreshCache(RedisKeys.getBusinessSystemOnlineKey(), statusMap);
    }

    private void refreshBackupAgents() {
        List<BackupAgentEntity> list = backupAgentMapper.selectList(new LambdaQueryWrapper<BackupAgentEntity>()
                .select(BackupAgentEntity::getInstance));
        Map<String, Object> statusMap = refreshWithThreads(list, BackupAgentEntity::getInstance, this::checkBackupAgentHealth);
        refreshCache(RedisKeys.getBackupAgentOnlineKey(), statusMap);
    }

    private void refreshDeviceBackups() {
        List<DeviceBackupEntity> list = deviceBackupMapper.selectList(new LambdaQueryWrapper<DeviceBackupEntity>()
                .select(DeviceBackupEntity::getInstance));
        Map<String, Object> statusMap = refreshWithThreads(list, DeviceBackupEntity::getInstance,
                instance -> PingUtils.isReachable(instance, 2000));
        refreshCache(RedisKeys.getDeviceBackupOnlineKey(), statusMap);
    }

    private void refreshCache(String key, Map<String, Object> statusMap) {
        redisUtils.delete(key);
        if (!statusMap.isEmpty()) {
            redisUtils.hMSet(key, statusMap, CACHE_EXPIRE_SECONDS);
        }
    }

    private <T> Map<String, Object> refreshWithThreads(List<T> list,
                                                       Function<T, String> instanceFn,
                                                       Function<String, Boolean> checker) {
        Map<String, Object> statusMap = new HashMap<>();
        if (list == null || list.isEmpty()) {
            return statusMap;
        }
        int poolSize = Math.min(50, Math.max(4, list.size()));
        ExecutorService executor = Executors.newFixedThreadPool(poolSize);
        try {
            List<Future<Map.Entry<String, Boolean>>> futures = list.stream()
                    .map(item -> (Callable<Map.Entry<String, Boolean>>) () -> {
                        String instance = instanceFn.apply(item);
                        if (StrUtil.isBlank(instance)) {
                            return null;
                        }
                        Boolean ok = checker.apply(instance);
                        return Map.entry(instance, ok);
                    })
                    .map(executor::submit)
                    .toList();

            for (Future<Map.Entry<String, Boolean>> future : futures) {
                try {
                    Map.Entry<String, Boolean> entry = future.get(2, TimeUnit.SECONDS);
                    if (entry != null) {
                        statusMap.put(entry.getKey(), entry.getValue());
                    }
                } catch (Exception ignore) {
                    // skip failed probe
                }
            }
        } finally {
            executor.shutdownNow();
        }
        return statusMap;
    }

    private boolean checkBackupAgentHealth(String instance) {
        if (StrUtil.isBlank(instance)) {
            return false;
        }
        String base = instance.trim();
        if (!base.startsWith("http://") && !base.startsWith("https://")) {
            base = "http://" + base;
        }
        String url = base.endsWith("/") ? (base + "health") : (base + "/health");
        try {
            java.net.HttpURLConnection conn = (java.net.HttpURLConnection) new java.net.URL(url).openConnection();
            conn.setRequestMethod("GET");
            conn.setConnectTimeout(2000);
            conn.setReadTimeout(2000);
            conn.connect();
            if (conn.getResponseCode() != 200) {
                return false;
            }
            try (java.io.InputStream in = conn.getInputStream()) {
                String body = new String(in.readAllBytes());
                return body.contains("\"status\":\"ok\"") || body.contains("\"status\" : \"ok\"") || body.contains("\"status\": \"ok\"");
            }
        } catch (Exception ignore) {
            return false;
        }
    }

}
