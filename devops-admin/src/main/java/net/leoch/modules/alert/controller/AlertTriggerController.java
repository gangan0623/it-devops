package net.leoch.modules.alert.controller;


import static net.leoch.common.core.base.Constant.LIMIT;
import static net.leoch.common.core.base.Constant.ORDER;
import static net.leoch.common.core.base.Constant.ORDER_FIELD;
import static net.leoch.common.core.base.Constant.PAGE;

import cn.dev33.satoken.annotation.SaCheckPermission;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import net.leoch.common.core.annotation.LogOperation;
import net.leoch.common.data.page.PageData;
import net.leoch.common.utils.Result;
import net.leoch.modules.alert.vo.req.AlertTriggerPageReq;
import net.leoch.modules.alert.vo.rsp.AlertTriggerRsp;
import net.leoch.modules.alert.vo.req.AlertTriggerReq;
import net.leoch.modules.alert.service.IAlertTriggerService;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

/**
 * 告警触发器
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@RestController
@RequestMapping("alert/trigger")
@Tag(name = "告警触发器")
@RequiredArgsConstructor
public class AlertTriggerController {

    private final IAlertTriggerService alertTriggerService;

    @GetMapping("page")
    @Operation(summary = "分页")
    @Parameters({
        @Parameter(name = Constant.PAGE, description = "当前页码，从1开始", in = ParameterIn.QUERY, required = true, ref="int") ,
        @Parameter(name = Constant.LIMIT, description = "每页显示记录数", in = ParameterIn.QUERY,required = true, ref="int") ,
        @Parameter(name = Constant.ORDER_FIELD, description = "排序字段", in = ParameterIn.QUERY, ref="String") ,
        @Parameter(name = Constant.ORDER, description = "排序方式，可选值(asc、desc)", in = ParameterIn.QUERY, ref="String")
    })
    @SaCheckPermission("alert:trigger:page")
    public Result<PageData<AlertTriggerRsp>> page(AlertTriggerPageReq request){
        return new Result<PageData<AlertTriggerRsp>>().ok(alertTriggerService.pageWithReceivers(request));
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @SaCheckPermission("alert:trigger:info")
    public Result<AlertTriggerRsp> get(@PathVariable("id") Long id){
        return new Result<AlertTriggerRsp>().ok(alertTriggerService.getWithReceivers(id));
    }

    @PostMapping
    @Operation(summary = "保存")
    @LogOperation("保存")
    @SaCheckPermission("alert:trigger:save")
    public Result<Object> save(@RequestBody AlertTriggerReq dto){
        alertTriggerService.save(dto);
        return new Result<>();
    }

    @PutMapping
    @Operation(summary = "修改")
    @LogOperation("修改")
    @SaCheckPermission("alert:trigger:update")
    public Result<Object> update(@RequestBody AlertTriggerReq dto){
        alertTriggerService.update(dto);
        return new Result<>();
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @SaCheckPermission("alert:trigger:delete")
    public Result<Object> delete(@RequestBody Long[] ids){
        alertTriggerService.delete(ids);
        return new Result<>();
    }

    @GetMapping("resources")
    @Operation(summary = "资源")
    @SaCheckPermission("alert:trigger:page")
    public Result<Map<String, Object>> resources() {
        return new Result<Map<String, Object>>().ok(alertTriggerService.resources());
    }

    @GetMapping("options")
    @Operation(summary = "触发器选项")
    @SaCheckPermission("alert:trigger:page")
    public Result<List<Map<String, Object>>> options() {
        return new Result<List<Map<String, Object>>>().ok(alertTriggerService.options());
    }
}
