package net.leoch.common.integration.schedule.task;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.modules.ops.service.ZabbixNetworkHostSyncService;
import org.springframework.stereotype.Component;

/**
 * Zabbix网络设备同步定时任务
 *
 * beanName: zabbixNetworkHostSyncTask
 */
@Slf4j
@Component("zabbixNetworkHostSyncTask")
@RequiredArgsConstructor
public class ZabbixNetworkHostSyncTask implements ITask {
    private final ZabbixNetworkHostSyncService syncService;

    @Override
    public void run(String params) {
        log.info("[Zabbix网络设备同步任务] 开始执行, params={}", params);
        var rsp = syncService.sync();
        log.info("[Zabbix网络设备同步任务] 完成, totalFetched={}, inserted={}, updated={}, disabled={}, skipped={}",
                rsp.getTotalFetched(), rsp.getInserted(), rsp.getUpdated(), rsp.getDisabled(), rsp.getSkipped());
    }
}
