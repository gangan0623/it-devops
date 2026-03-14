package net.leoch.modules.alert.controller;

import cn.dev33.satoken.annotation.SaCheckPermission;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import net.leoch.common.data.result.Result;
import net.leoch.modules.alert.service.AlertReportAsyncService;
import net.leoch.modules.alert.service.PrometheusAlertReportService;
import net.leoch.modules.alert.vo.rsp.PrometheusAlertAiReportRsp;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequestMapping("alert/prometheus/report")
@Tag(name = "Prometheus告警AI报告")
@RequiredArgsConstructor
public class PrometheusAlertReportController {

    private final PrometheusAlertReportService prometheusAlertReportService;
    private final AlertReportAsyncService alertReportAsyncService;

    @GetMapping("latest")
    @Operation(summary = "最新报告列表")
    @SaCheckPermission("alert:record:page")
    public Result<List<PrometheusAlertAiReportRsp>> latest(
            @RequestParam(value = "reportType", required = false) String reportType,
            @RequestParam(value = "size", required = false, defaultValue = "20") Integer size) {
        return new Result<List<PrometheusAlertAiReportRsp>>().ok(
                prometheusAlertReportService.latest(reportType, size));
    }

    @GetMapping("{id}")
    @Operation(summary = "报告详情")
    @SaCheckPermission("alert:record:page")
    public Result<PrometheusAlertAiReportRsp> get(@PathVariable("id") Long id) {
        return new Result<PrometheusAlertAiReportRsp>().ok(prometheusAlertReportService.getById(id));
    }

    @PostMapping("generate")
    @Operation(summary = "生成报告")
    @SaCheckPermission("alert:record:page")
    public Result<PrometheusAlertAiReportRsp> generate(
            @RequestParam(value = "reportType", required = false, defaultValue = "server") String reportType,
            @RequestParam(value = "periodType", required = false, defaultValue = "week") String periodType) {
        PrometheusAlertAiReportRsp rsp = prometheusAlertReportService.submitGenerate(reportType, periodType, null, null);
        alertReportAsyncService.generatePrometheusReport(rsp.getId());
        return new Result<PrometheusAlertAiReportRsp>().ok(rsp);
    }
}
