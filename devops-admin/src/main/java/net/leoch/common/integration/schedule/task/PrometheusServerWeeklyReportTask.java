package net.leoch.common.integration.schedule.task;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.modules.alert.service.PrometheusAlertReportService;
import org.springframework.stereotype.Component;

/**
 * beanName: prometheusServerWeeklyReportTask
 */
@Slf4j
@Component("prometheusServerWeeklyReportTask")
@RequiredArgsConstructor
public class PrometheusServerWeeklyReportTask implements ITask {

    private final PrometheusAlertReportService prometheusAlertReportService;

    @Override
    public void run(String params) {
        prometheusAlertReportService.generate("server", "week", null, null);
        log.info("[PrometheusServerWeeklyReportTask] weekly report generated");
    }
}
