package net.leoch.common.integration.schedule.task;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.support.SFunction;
import net.leoch.common.utils.ops.MetricsUtils;
import net.leoch.common.utils.ops.PingUtils;
import net.leoch.framework.config.ops.OnlineStatusConfig;
import net.leoch.modules.ops.entity.*;
import net.leoch.modules.ops.mapper.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.ArrayList;
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

    private final LinuxHostMapper linuxHostMapper;
    private final WindowHostMapper windowHostMapper;
    private final BusinessSystemMapper businessSystemMapper;
    private final BackupAgentMapper backupAgentMapper;
    private final DeviceBackupMapper deviceBackupMapper;
    private final OnlineStatusConfig properties;

    public OnlineStatusRefreshTask(LinuxHostMapper linuxHostMapper,
                                   WindowHostMapper windowHostMapper,
                                   BusinessSystemMapper businessSystemMapper,
                                   BackupAgentMapper backupAgentMapper,
                                   DeviceBackupMapper deviceBackupMapper,
                                   OnlineStatusConfig properties) {
        this.linuxHostMapper = linuxHostMapper;
        this.windowHostMapper = windowHostMapper;
        this.businessSystemMapper = businessSystemMapper;
        this.backupAgentMapper = backupAgentMapper;
        this.deviceBackupMapper = deviceBackupMapper;
        this.properties = properties;
    }

    @Override
    public void run(String params) {
        long start = System.currentTimeMillis();
        refreshLinux();
        refreshWindows();
        refreshBusinessSystems();
        refreshBackupAgents();
        refreshDeviceBackups();
        logger.info("[在线状态刷新] 完成, 耗时={}ms", System.currentTimeMillis() - start);
    }

    private void refreshLinux() {
        List<LinuxHostEntity> list = linuxHostMapper.selectList(new LambdaQueryWrapper<LinuxHostEntity>()
                .select(LinuxHostEntity::getInstance));
        Map<String, Object> statusMap = refreshWithThreads(list, LinuxHostEntity::getInstance,
                instance -> MetricsUtils.metricsOk(instance, properties.getOnlineStatus().getTimeout().getMetrics()));
        refreshTableStatus(linuxHostMapper, LinuxHostEntity::getOnlineStatus, LinuxHostEntity::getInstance, statusMap);
    }

    private void refreshWindows() {
        List<WindowHostEntity> list = windowHostMapper.selectList(new LambdaQueryWrapper<WindowHostEntity>()
                .select(WindowHostEntity::getInstance));
        Map<String, Object> statusMap = refreshWithThreads(list, WindowHostEntity::getInstance,
                instance -> MetricsUtils.metricsOk(instance, properties.getOnlineStatus().getTimeout().getMetrics()));
        refreshTableStatus(windowHostMapper, WindowHostEntity::getOnlineStatus, WindowHostEntity::getInstance, statusMap);
    }

    private void refreshBusinessSystems() {
        List<BusinessSystemEntity> list = businessSystemMapper.selectList(new LambdaQueryWrapper<BusinessSystemEntity>()
                .select(BusinessSystemEntity::getInstance));
        Map<String, Object> statusMap = refreshWithThreads(list, BusinessSystemEntity::getInstance,
                instance -> PingUtils.isReachable(instance, properties.getOnlineStatus().getTimeout().getPing()));
        refreshTableStatus(businessSystemMapper, BusinessSystemEntity::getOnlineStatus, BusinessSystemEntity::getInstance, statusMap);
    }

    private void refreshBackupAgents() {
        List<BackupAgentEntity> list = backupAgentMapper.selectList(new LambdaQueryWrapper<BackupAgentEntity>()
                .select(BackupAgentEntity::getInstance));
        Map<String, Object> statusMap = refreshWithThreads(list, BackupAgentEntity::getInstance, this::checkBackupAgentHealth);
        refreshTableStatus(backupAgentMapper, BackupAgentEntity::getOnlineStatus, BackupAgentEntity::getInstance, statusMap);
    }

    private void refreshDeviceBackups() {
        List<DeviceBackupEntity> list = deviceBackupMapper.selectList(new LambdaQueryWrapper<DeviceBackupEntity>()
                .select(DeviceBackupEntity::getInstance));
        Map<String, Object> statusMap = refreshWithThreads(list, DeviceBackupEntity::getInstance,
                instance -> PingUtils.isReachable(instance, properties.getOnlineStatus().getTimeout().getDevice()));
        refreshTableStatus(deviceBackupMapper, DeviceBackupEntity::getOnlineStatus, DeviceBackupEntity::getInstance, statusMap);
    }

    private static final int UPDATE_BATCH_SIZE = 500;

    private <T> void refreshTableStatus(BaseMapper<T> mapper,
                                        SFunction<T, ?> onlineStatusColumn,
                                        SFunction<T, ?> instanceColumn,
                                        Map<String, Object> statusMap) {
        mapper.update(null, new LambdaUpdateWrapper<T>()
                .set(onlineStatusColumn, false)
                .isNotNull(instanceColumn));
        if (statusMap == null || statusMap.isEmpty()) {
            return;
        }
        List<String> onlineInstances = new ArrayList<>();
        for (Map.Entry<String, Object> entry : statusMap.entrySet()) {
            String instance = entry.getKey();
            if (StrUtil.isBlank(instance)) {
                continue;
            }
            Integer dbStatus = toDbStatus(entry.getValue());
            if (Integer.valueOf(1).equals(dbStatus)) {
                onlineInstances.add(instance);
            }
        }
        batchUpdateByInstances(mapper, onlineStatusColumn, instanceColumn, onlineInstances, true);
    }

    private <T> void batchUpdateByInstances(BaseMapper<T> mapper,
                                            SFunction<T, ?> onlineStatusColumn,
                                            SFunction<T, ?> instanceColumn,
                                            List<String> instances,
                                            Boolean status) {
        if (instances == null || instances.isEmpty()) {
            return;
        }
        for (int i = 0; i < instances.size(); i += UPDATE_BATCH_SIZE) {
            List<String> batch = instances.subList(i, Math.min(i + UPDATE_BATCH_SIZE, instances.size()));
            mapper.update(null, new LambdaUpdateWrapper<T>()
                    .set(onlineStatusColumn, status)
                    .in(instanceColumn, batch));
        }
    }

    private Integer toDbStatus(Object value) {
        if (value == null) {
            return null;
        }
        if (value instanceof Boolean bool) {
            return bool ? 1 : 0;
        }
        if (value instanceof Number number) {
            return number.intValue() == 0 ? 0 : 1;
        }
        String str = String.valueOf(value);
        if ("true".equalsIgnoreCase(str) || "1".equals(str)) {
            return 1;
        }
        if ("false".equalsIgnoreCase(str) || "0".equals(str)) {
            return 0;
        }
        return null;
    }

    private <T> Map<String, Object> refreshWithThreads(List<T> list,
                                                       Function<T, String> instanceFn,
                                                       Function<String, Boolean> checker) {
        Map<String, Object> statusMap = new HashMap<>();
        if (list == null || list.isEmpty()) {
            return statusMap;
        }
        int poolSize = Math.min(properties.getOnlineStatus().getThreadPool().getMaxSize(),
                Math.max(properties.getOnlineStatus().getThreadPool().getCoreSize(), list.size()));
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

            int futureTimeout = properties.getOnlineStatus().getTimeout().getFuture();
            for (Future<Map.Entry<String, Boolean>> future : futures) {
                try {
                    Map.Entry<String, Boolean> entry = future.get(futureTimeout, TimeUnit.MILLISECONDS);
                    if (entry != null) {
                        statusMap.put(entry.getKey(), entry.getValue());
                    }
                } catch (TimeoutException e) {
                    logger.debug("[在线状态刷新] 探测超时, timeout={}ms", futureTimeout);
                } catch (Exception e) {
                    logger.debug("[在线状态刷新] 探测异常", e);
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
        String url = base.endsWith("/") ? (base + "healthz") : (base + "/healthz");
        int timeout = properties.getOnlineStatus().getTimeout().getAgent();
        try {
            java.net.HttpURLConnection conn = (java.net.HttpURLConnection) new java.net.URL(url).openConnection();
            conn.setRequestMethod("GET");
            conn.setConnectTimeout(timeout);
            conn.setReadTimeout(timeout);
            conn.connect();
            if (conn.getResponseCode() != 200) {
                return false;
            }
            try (java.io.InputStream in = conn.getInputStream()) {
                String body = new String(in.readAllBytes());
                return body.contains("\"status\":\"ok\"") || body.contains("\"status\" : \"ok\"") || body.contains("\"status\": \"ok\"");
            }
        } catch (Exception e) {
            logger.debug("[备份代理健康检查] 检查失败, url={}, timeout={}ms", url, timeout);
            return false;
        }
    }

}
