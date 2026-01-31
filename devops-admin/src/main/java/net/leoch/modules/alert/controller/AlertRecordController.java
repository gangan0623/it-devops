package net.leoch.modules.alert.controller;

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
import net.leoch.modules.alert.dto.AlertRecordDTO;
import net.leoch.modules.alert.service.AlertRecordService;
import net.leoch.modules.alert.service.AlertSseService;
import org.apache.shiro.authz.annotation.RequiresPermissions;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.util.Map;

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
    @RequiresPermissions("alert:record:page")
    public Result<PageData<AlertRecordDTO>> page(@Parameter(hidden = true) @RequestParam Map<String, Object> params){
        PageData<AlertRecordDTO> page = alertRecordService.page(params);

        return new Result<PageData<AlertRecordDTO>>().ok(page);
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @RequiresPermissions("alert:record:info")
    public Result<AlertRecordDTO> get(@PathVariable("id") Long id){
        AlertRecordDTO data = alertRecordService.get(id);

        return new Result<AlertRecordDTO>().ok(data);
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @RequiresPermissions("alert:record:delete")
    public Result<Object> delete(@RequestBody Long[] ids){
        AssertUtils.isArrayEmpty(ids, "id");
        alertRecordService.delete(ids);

        return new Result<>();
    }

    @GetMapping("stream")
    @Operation(summary = "实时告警SSE")
    @RequiresPermissions("alert:record:page")
    public SseEmitter stream(@RequestParam(value = "token", required = false) String token) {
        return alertSseService.createEmitter();
    }
}
