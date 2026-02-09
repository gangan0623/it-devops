package net.leoch.modules.alert.controller;


import static net.leoch.common.constant.Constant.LIMIT;
import static net.leoch.common.constant.Constant.ORDER;
import static net.leoch.common.constant.Constant.ORDER_FIELD;
import static net.leoch.common.constant.Constant.PAGE;

import cn.dev33.satoken.annotation.SaCheckPermission;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.tags.Tag;
import net.leoch.common.annotation.LogOperation;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.Result;
import net.leoch.common.validator.AssertUtils;
import net.leoch.modules.alert.vo.rsp.AlertRecordActionRsp;
import net.leoch.modules.alert.vo.req.AlertProblemPageReq;
import net.leoch.modules.alert.vo.req.AlertRecordActionReq;
import net.leoch.modules.alert.vo.rsp.AlertProblemRsp;
import net.leoch.modules.alert.vo.rsp.AlertRecordRsp;
import net.leoch.modules.alert.vo.req.AlertRecordPageReq;
import net.leoch.modules.alert.service.IAlertRecordService;
import net.leoch.modules.alert.service.IAlertSseService;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.util.List;

/**
 * 告警记录
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@RestController
@RequestMapping("alert/record")
@Tag(name = "告警记录")
public class AlertRecordController {

    private final IAlertRecordService alertRecordService;
    private final IAlertSseService alertSseService;

    public AlertRecordController(IAlertRecordService alertRecordService, IAlertSseService alertSseService) {
        this.alertRecordService = alertRecordService;
        this.alertSseService = alertSseService;
    }

    @GetMapping("page")
    @Operation(summary = "分页")
    @Parameters({
        @Parameter(name = Constant.PAGE, description = "当前页码，从1开始", in = ParameterIn.QUERY, required = true, ref="int") ,
        @Parameter(name = Constant.LIMIT, description = "每页显示记录数", in = ParameterIn.QUERY,required = true, ref="int") ,
        @Parameter(name = Constant.ORDER_FIELD, description = "排序字段", in = ParameterIn.QUERY, ref="String") ,
        @Parameter(name = Constant.ORDER, description = "排序方式，可选值(asc、desc)", in = ParameterIn.QUERY, ref="String")
    })
    @SaCheckPermission("alert:record:page")
    public Result<PageData<AlertRecordRsp>> page(AlertRecordPageReq request){
        PageData<AlertRecordRsp> page = alertRecordService.page(request);

        return new Result<PageData<AlertRecordRsp>>().ok(page);
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @SaCheckPermission("alert:record:info")
    public Result<AlertRecordRsp> get(@PathVariable("id") Long id){
        AlertRecordRsp data = alertRecordService.get(id);

        return new Result<AlertRecordRsp>().ok(data);
    }

    @GetMapping("problem/page")
    @Operation(summary = "问题分页")
    @SaCheckPermission("alert:problem:page")
    public Result<PageData<AlertProblemRsp>> problemPage(AlertProblemPageReq request) {
        return new Result<PageData<AlertProblemRsp>>().ok(alertRecordService.problemPage(request));
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @SaCheckPermission("alert:record:delete")
    public Result<Object> delete(@RequestBody Long[] ids){
        AssertUtils.isArrayEmpty(ids, "id");
        alertRecordService.delete(ids);

        return new Result<>();
    }

    @GetMapping("{id}/action/history")
    @Operation(summary = "操作历史")
    @SaCheckPermission("alert:record:info")
    public Result<List<AlertRecordActionRsp>> history(@PathVariable("id") Long id) {
        return new Result<List<AlertRecordActionRsp>>().ok(alertRecordService.history(id));
    }

    @PostMapping("{id}/action/severity")
    @Operation(summary = "更改严重性")
    @LogOperation("告警更改严重性")
    @SaCheckPermission("alert:record:info")
    public Result<Object> changeSeverity(@PathVariable("id") Long id, @RequestBody AlertRecordActionReq request) {
        alertRecordService.changeSeverity(id, request == null ? null : request.getSeverity(), request == null ? null : request.getMessage());
        return new Result<>();
    }

    @PostMapping("{id}/action/suppress")
    @Operation(summary = "抑制")
    @LogOperation("告警抑制")
    @SaCheckPermission("alert:record:info")
    public Result<Object> suppress(@PathVariable("id") Long id, @RequestBody AlertRecordActionReq request) {
        alertRecordService.suppress(id, request == null ? null : request.getDays(), request == null ? null : request.getMessage());
        return new Result<>();
    }

    @PostMapping("{id}/action/ack")
    @Operation(summary = "确定")
    @LogOperation("告警确定")
    @SaCheckPermission("alert:record:info")
    public Result<Object> acknowledge(@PathVariable("id") Long id, @RequestBody AlertRecordActionReq request) {
        alertRecordService.acknowledge(id, request == null ? null : request.getMessage());
        return new Result<>();
    }

    @PostMapping("{id}/action/close")
    @Operation(summary = "关闭")
    @LogOperation("告警关闭")
    @SaCheckPermission("alert:record:info")
    public Result<Object> close(@PathVariable("id") Long id, @RequestBody AlertRecordActionReq request) {
        alertRecordService.close(id, request == null ? null : request.getMessage());
        return new Result<>();
    }

    @GetMapping("stream")
    @Operation(summary = "实时告警SSE")
    @SaCheckPermission("alert:record:page")
    public SseEmitter stream(@RequestParam(value = "token", required = false) String token) {
        return alertSseService.createEmitter();
    }
}
