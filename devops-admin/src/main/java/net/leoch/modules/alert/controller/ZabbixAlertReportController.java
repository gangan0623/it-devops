package net.leoch.modules.alert.controller;

import cn.dev33.satoken.annotation.SaCheckPermission;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import net.leoch.common.data.result.Result;
import net.leoch.modules.alert.service.AlertReportAsyncService;
import net.leoch.modules.alert.service.ZabbixAlertReportService;
import net.leoch.modules.alert.vo.req.ZabbixAlertReportGenerateReq;
import net.leoch.modules.alert.vo.rsp.AlertAiReportZabbixRsp;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequestMapping("alert/zabbix/report")
@Tag(name = "Zabbix告警AI报告")
@RequiredArgsConstructor
public class ZabbixAlertReportController {

    private final ZabbixAlertReportService zabbixAlertReportService;
    private final AlertReportAsyncService alertReportAsyncService;

    @GetMapping("latest")
    @Operation(summary = "最新报告列表")
    @SaCheckPermission("alert:record:page")
    public Result<List<AlertAiReportZabbixRsp>> latest(@RequestParam(value = "size", required = false, defaultValue = "20") Integer size) {
        return new Result<List<AlertAiReportZabbixRsp>>().ok(zabbixAlertReportService.latest(size));
    }

    @GetMapping("{id}")
    @Operation(summary = "报告详情")
    @SaCheckPermission("alert:record:page")
    public Result<AlertAiReportZabbixRsp> get(@PathVariable("id") Long id) {
        return new Result<AlertAiReportZabbixRsp>().ok(zabbixAlertReportService.getById(id));
    }

    @PostMapping("generate")
    @Operation(summary = "生成报告")
    @SaCheckPermission("alert:record:page")
    public Result<AlertAiReportZabbixRsp> generate(@RequestBody(required = false) ZabbixAlertReportGenerateReq req) {
        ZabbixAlertReportGenerateReq request = req == null ? new ZabbixAlertReportGenerateReq() : req;
        AlertAiReportZabbixRsp rsp = zabbixAlertReportService.submitGenerate(
                request.getPeriodType(), request.getPeriodStart(), request.getPeriodEnd()
        );
        alertReportAsyncService.generateZabbixReport(rsp.getId());
        return new Result<AlertAiReportZabbixRsp>().ok(rsp);
    }
}
