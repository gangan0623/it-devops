package net.leoch.common.integration.schedule.task;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.toolkit.support.SFunction;
import net.leoch.framework.config.ops.OnlineStatusConfig;
import net.leoch.modules.ops.service.PrometheusOnlineStatusService;
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
    private final DomainRecordMapper domainRecordMapper;
    private final NetworkBackupAgentMapper backupAgentMapper;
    private final OnlineStatusConfig properties;
    private final PrometheusOnlineStatusService prometheusOnlineStatusService;

    public OnlineStatusRefreshTask(LinuxHostMapper linuxHostMapper,
                                   WindowHostMapper windowHostMapper,
                                   DomainRecordMapper domainRecordMapper,
                                   NetworkBackupAgentMapper backupAgentMapper,
                                   OnlineStatusConfig properties,
                                   PrometheusOnlineStatusService prometheusOnlineStatusService) {
        this.linuxHostMapper = linuxHostMapper;
        this.windowHostMapper = windowHostMapper;
        this.domainRecordMapper = domainRecordMapper;
        this.backupAgentMapper = backupAgentMapper;
        this.properties = properties;
        this.prometheusOnlineStatusService = prometheusOnlineStatusService;
    }

    @Override
    public void run(String params) {
        long start = System.currentTimeMillis();
        refreshLinux();
        refreshWindows();
        refreshDomainRecords();
        refreshBackupAgents();
        logger.info("[在线状态刷新] 完成, 耗时={}ms", System.currentTimeMillis() - start);
    }

    private void refreshLinux() {
        PrometheusOnlineStatusService.BatchStatusResult result =
                prometheusOnlineStatusService.queryJobStatus(PrometheusOnlineStatusService.JOB_LINUX);
        if (!result.success()) {
            logger.warn("[在线状态刷新] Linux Prometheus 查询失败，跳过本轮状态覆盖");
            return;
        }
        refreshTableStatus(linuxHostMapper, LinuxHostEntity::getOnlineStatus, LinuxHostEntity::getInstance,
                LinuxHostEntity::getInstance, result.statusMap(), PrometheusOnlineStatusService.JOB_LINUX);
    }

    private void refreshWindows() {
        PrometheusOnlineStatusService.BatchStatusResult result =
                prometheusOnlineStatusService.queryJobStatus(PrometheusOnlineStatusService.JOB_WINDOWS);
        if (!result.success()) {
            logger.warn("[在线状态刷新] Windows Prometheus 查询失败，跳过本轮状态覆盖");
            return;
        }
        refreshTableStatus(windowHostMapper, WindowHostEntity::getOnlineStatus, WindowHostEntity::getInstance,
                WindowHostEntity::getInstance, result.statusMap(), PrometheusOnlineStatusService.JOB_WINDOWS);
    }

    private void refreshDomainRecords() {
        PrometheusOnlineStatusService.BatchStatusResult result =
                prometheusOnlineStatusService.queryJobStatus(PrometheusOnlineStatusService.JOB_HTTP_PROBE);
        if (!result.success()) {
            logger.warn("[在线状态刷新] DomainRecord Prometheus 查询失败，跳过本轮状态覆盖");
            return;
        }
        refreshTableStatus(domainRecordMapper, DomainRecordEntity::getOnlineStatus, DomainRecordEntity::getApiUrl,
                DomainRecordEntity::getApiUrl, result.statusMap(), PrometheusOnlineStatusService.JOB_HTTP_PROBE);
    }

    private void refreshBackupAgents() {
        List<NetworkBackupAgentEntity> list = backupAgentMapper.selectList(new LambdaQueryWrapper<NetworkBackupAgentEntity>()
                .select(NetworkBackupAgentEntity::getInstance));
        Map<String, Object> statusMap = refreshWithThreads(list, NetworkBackupAgentEntity::getInstance, this::checkBackupAgentHealth);
        refreshTableStatus(backupAgentMapper, NetworkBackupAgentEntity::getOnlineStatus, NetworkBackupAgentEntity::getInstance, statusMap);
    }

    private static final int UPDATE_BATCH_SIZE = 500;

    private <T> void refreshTableStatus(BaseMapper<T> mapper,
                                        SFunction<T, ?> onlineStatusColumn,
                                        SFunction<T, ?> instanceColumn,
                                        Function<T, String> instanceGetter,
                                        Map<String, Boolean> statusMap,
                                        String job) {
        mapper.update(null, new LambdaUpdateWrapper<T>()
                .set(onlineStatusColumn, false)
                .isNotNull(instanceColumn));
        if (statusMap == null || statusMap.isEmpty()) {
            return;
        }
        List<T> entities = mapper.selectList(new LambdaQueryWrapper<T>()
                .select(instanceColumn)
                .isNotNull(instanceColumn));
        List<String> onlineInstances = new ArrayList<>();
        for (T entity : entities) {
            String instance = instanceGetter.apply(entity);
            if (StrUtil.isBlank(instance)) {
                continue;
            }
            if (prometheusOnlineStatusService.matchesOnline(statusMap, job, instance)) {
                onlineInstances.add(instance);
            }
        }
        batchUpdateByInstances(mapper, onlineStatusColumn, instanceColumn, onlineInstances, true);
    }

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
            if (StrUtil.isBlank(entry.getKey())) {
                continue;
            }
            Object value = entry.getValue();
            if (Boolean.TRUE.equals(value)) {
                onlineInstances.add(entry.getKey());
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
