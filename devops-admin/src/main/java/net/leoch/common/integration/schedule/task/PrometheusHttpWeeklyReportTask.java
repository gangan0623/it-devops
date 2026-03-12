package net.leoch.common.integration.schedule.task;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.modules.alert.service.PrometheusAlertReportService;
import org.springframework.stereotype.Component;

/**
 * beanName: prometheusHttpWeeklyReportTask
 */
@Slf4j
@Component("prometheusHttpWeeklyReportTask")
@RequiredArgsConstructor
public class PrometheusHttpWeeklyReportTask implements ITask {

    private final PrometheusAlertReportService prometheusAlertReportService;

    @Override
    public void run(String params) {
        prometheusAlertReportService.generate("http", "week", null, null);
        log.info("[PrometheusHttpWeeklyReportTask] weekly report generated");
    }
}
