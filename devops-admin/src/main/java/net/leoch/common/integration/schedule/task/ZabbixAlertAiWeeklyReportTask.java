package net.leoch.common.integration.schedule.task;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.modules.alert.service.ZabbixAlertReportService;
import org.springframework.stereotype.Component;

/**
 * beanName: zabbixAlertAiWeeklyReportTask
 */
@Slf4j
@Component("zabbixAlertAiWeeklyReportTask")
@RequiredArgsConstructor
public class ZabbixAlertAiWeeklyReportTask implements ITask {

    private final ZabbixAlertReportService zabbixAlertReportService;

    @Override
    public void run(String params) {
        zabbixAlertReportService.generate("week", null, null);
        log.info("[ZabbixAIReportTask] weekly report generated");
    }
}
