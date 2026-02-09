package net.leoch.modules.alert.controller;

import static net.leoch.common.constant.Constant.LIMIT;
import static net.leoch.common.constant.Constant.ORDER;
import static net.leoch.common.constant.Constant.ORDER_FIELD;
import static net.leoch.common.constant.Constant.PAGE;

import cn.dev33.satoken.annotation.SaCheckPermission;
import cn.hutool.core.util.StrUtil;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.tags.Tag;
import net.leoch.common.annotation.LogOperation;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.Result;
import net.leoch.common.validator.AssertUtils;
import net.leoch.modules.alert.vo.rsp.AlertMediaRsp;
import net.leoch.modules.alert.vo.req.AlertMediaPageReq;
import net.leoch.modules.alert.vo.req.AlertMediaReq;
import net.leoch.modules.alert.vo.req.AlertMediaTestReq;
import net.leoch.modules.alert.entity.AlertMediaEntity;
import net.leoch.modules.alert.service.AlertMailService;
import net.leoch.modules.alert.service.IAlertMediaService;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 告警媒介
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@RestController
@RequestMapping("alert/media")
@Tag(name = "告警媒介")
public class AlertMediaController {

    private final IAlertMediaService alertMediaService;
    private final AlertMailService alertMailService;

    public AlertMediaController(IAlertMediaService alertMediaService, AlertMailService alertMailService) {
        this.alertMediaService = alertMediaService;
        this.alertMailService = alertMailService;
    }

    @GetMapping("page")
    @Operation(summary = "分页")
    @Parameters({
        @Parameter(name = PAGE, description = "当前页码，从1开始", in = ParameterIn.QUERY, required = true, ref="int") ,
        @Parameter(name = LIMIT, description = "每页显示记录数", in = ParameterIn.QUERY,required = true, ref="int") ,
        @Parameter(name = ORDER_FIELD, description = "排序字段", in = ParameterIn.QUERY, ref="String") ,
        @Parameter(name = ORDER, description = "排序方式，可选值(asc、desc)", in = ParameterIn.QUERY, ref="String")
    })
    @SaCheckPermission("alert:media:page")
    public Result<PageData<AlertMediaRsp>> page(AlertMediaPageReq request){
        return new Result<PageData<AlertMediaRsp>>().ok(alertMediaService.page(request));
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @SaCheckPermission("alert:media:info")
    public Result<AlertMediaRsp> get(@PathVariable("id") Long id){
        AlertMediaRsp data = alertMediaService.get(id);

        return new Result<AlertMediaRsp>().ok(data);
    }

    @PostMapping
    @Operation(summary = "保存")
    @LogOperation("保存")
    @SaCheckPermission("alert:media:save")
    public Result<Object> save(@RequestBody AlertMediaReq dto){
        alertMediaService.save(dto);

        return new Result<>();
    }

    @PutMapping
    @Operation(summary = "修改")
    @LogOperation("修改")
    @SaCheckPermission("alert:media:update")
    public Result<Object> update(@RequestBody AlertMediaReq dto){
        alertMediaService.update(dto);

        return new Result<>();
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @SaCheckPermission("alert:media:delete")
    public Result<Object> delete(@RequestBody Long[] ids){
        AssertUtils.isArrayEmpty(ids, "id");
        alertMediaService.delete(ids);

        return new Result<>();
    }

    @PostMapping("test")
    @Operation(summary = "媒介测试")
    @SaCheckPermission("alert:media:test")
    public Result<Object> test(@RequestBody AlertMediaTestReq request) {
        alertMediaService.testMedia(request);
        return new Result<>();
    }
}
