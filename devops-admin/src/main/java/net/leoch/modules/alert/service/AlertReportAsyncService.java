package net.leoch.modules.alert.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class AlertReportAsyncService {
    private final ZabbixAlertReportService zabbixAlertReportService;
    private final PrometheusAlertReportService prometheusAlertReportService;

    @Async("reportTaskExecutor")
    public void generateZabbixReport(Long reportId) {
        try {
            zabbixAlertReportService.processReport(reportId);
        } catch (Exception e) {
            log.error("[ZabbixAIReport] async generate failed, reportId={}", reportId, e);
        }
    }

    @Async("reportTaskExecutor")
    public void generatePrometheusReport(Long reportId) {
        try {
            prometheusAlertReportService.processReport(reportId);
        } catch (Exception e) {
            log.error("[PrometheusAIReport] async generate failed, reportId={}", reportId, e);
        }
    }
}
