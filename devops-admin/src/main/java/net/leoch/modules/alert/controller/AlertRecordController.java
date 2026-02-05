package net.leoch.modules.alert.controller;

import cn.dev33.satoken.annotation.SaCheckPermission;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.tags.Tag;
import net.leoch.common.annotation.LogOperation;
import net.leoch.common.constant.Constant;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.Result;
import net.leoch.common.validator.AssertUtils;
import net.leoch.modules.alert.dto.AlertRecordActionDTO;
import net.leoch.modules.alert.dto.AlertRecordActionRequest;
import net.leoch.modules.alert.dto.AlertProblemDTO;
import net.leoch.modules.alert.dto.AlertRecordDTO;
import net.leoch.modules.alert.service.AlertRecordService;
import net.leoch.modules.alert.service.AlertSseService;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.util.Map;
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

    private final AlertRecordService alertRecordService;
    private final AlertSseService alertSseService;

    public AlertRecordController(AlertRecordService alertRecordService, AlertSseService alertSseService) {
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
    public Result<PageData<AlertRecordDTO>> page(@Parameter(hidden = true) @RequestParam Map<String, Object> params){
        PageData<AlertRecordDTO> page = alertRecordService.page(params);

        return new Result<PageData<AlertRecordDTO>>().ok(page);
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @SaCheckPermission("alert:record:info")
    public Result<AlertRecordDTO> get(@PathVariable("id") Long id){
        AlertRecordDTO data = alertRecordService.get(id);

        return new Result<AlertRecordDTO>().ok(data);
    }

    @GetMapping("problem/page")
    @Operation(summary = "问题分页")
    @Parameters({
        @Parameter(name = Constant.PAGE, description = "当前页码，从1开始", in = ParameterIn.QUERY, required = true, ref="int") ,
        @Parameter(name = Constant.LIMIT, description = "每页显示记录数", in = ParameterIn.QUERY,required = true, ref="int")
    })
    @SaCheckPermission("alert:problem:page")
    public Result<PageData<AlertProblemDTO>> problemPage(@Parameter(hidden = true) @RequestParam Map<String, Object> params) {
        return new Result<PageData<AlertProblemDTO>>().ok(alertRecordService.problemPage(params));
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
    public Result<List<AlertRecordActionDTO>> history(@PathVariable("id") Long id) {
        return new Result<List<AlertRecordActionDTO>>().ok(alertRecordService.history(id));
    }

    @PostMapping("{id}/action/severity")
    @Operation(summary = "更改严重性")
    @LogOperation("告警更改严重性")
    @SaCheckPermission("alert:record:info")
    public Result<Object> changeSeverity(@PathVariable("id") Long id, @RequestBody AlertRecordActionRequest request) {
        alertRecordService.changeSeverity(id, request == null ? null : request.getSeverity(), request == null ? null : request.getMessage());
        return new Result<>();
    }

    @PostMapping("{id}/action/suppress")
    @Operation(summary = "抑制")
    @LogOperation("告警抑制")
    @SaCheckPermission("alert:record:info")
    public Result<Object> suppress(@PathVariable("id") Long id, @RequestBody AlertRecordActionRequest request) {
        alertRecordService.suppress(id, request == null ? null : request.getDays(), request == null ? null : request.getMessage());
        return new Result<>();
    }

    @PostMapping("{id}/action/ack")
    @Operation(summary = "确定")
    @LogOperation("告警确定")
    @SaCheckPermission("alert:record:info")
    public Result<Object> acknowledge(@PathVariable("id") Long id, @RequestBody AlertRecordActionRequest request) {
        alertRecordService.acknowledge(id, request == null ? null : request.getMessage());
        return new Result<>();
    }

    @PostMapping("{id}/action/close")
    @Operation(summary = "关闭")
    @LogOperation("告警关闭")
    @SaCheckPermission("alert:record:info")
    public Result<Object> close(@PathVariable("id") Long id, @RequestBody AlertRecordActionRequest request) {
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
