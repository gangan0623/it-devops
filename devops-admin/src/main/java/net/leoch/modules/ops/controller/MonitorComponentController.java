package net.leoch.modules.ops.controller;


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
import net.leoch.common.page.PageData;
import net.leoch.common.utils.Result;
import net.leoch.modules.ops.vo.req.*;
import net.leoch.modules.ops.vo.rsp.*;
import net.leoch.modules.ops.service.IMonitorComponentService;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 监控组件
 */
@RestController
@RequestMapping("ops/monitorcomponent")
@Tag(name = "监控组件")
@RequiredArgsConstructor
public class MonitorComponentController {
    private final IMonitorComponentService monitorComponentService;

    @GetMapping("page")
    @Operation(summary = "分页")
    @Parameters({
        @Parameter(name = Constant.PAGE, description = "当前页码，从1开始", in = ParameterIn.QUERY, required = true, ref="int"),
        @Parameter(name = Constant.LIMIT, description = "每页显示记录数", in = ParameterIn.QUERY, required = true, ref="int"),
        @Parameter(name = Constant.ORDER_FIELD, description = "排序字段", in = ParameterIn.QUERY, ref="String"),
        @Parameter(name = Constant.ORDER, description = "排序方式，可选值(asc、desc)", in = ParameterIn.QUERY, ref="String")
    })
    @SaCheckPermission("ops:monitorcomponent:page")
    public Result<PageData<MonitorComponentRsp>> page(@Parameter(hidden = true) MonitorComponentPageReq request) {
        return new Result<PageData<MonitorComponentRsp>>().ok(monitorComponentService.page(request));
    }

    @GetMapping("list")
    @Operation(summary = "列表")
    @SaCheckPermission("ops:monitorcomponent:page")
    public Result<List<MonitorComponentRsp>> list(MonitorComponentListReq request) {
        return new Result<List<MonitorComponentRsp>>().ok(monitorComponentService.list(request));
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @SaCheckPermission("ops:monitorcomponent:info")
    public Result<MonitorComponentRsp> get(@PathVariable("id") Long id) {
        return new Result<MonitorComponentRsp>().ok(monitorComponentService.get(MonitorComponentIdReq.of(id)));
    }

    @PostMapping
    @Operation(summary = "保存")
    @LogOperation("保存")
    @SaCheckPermission("ops:monitorcomponent:save")
    public Result<Object> save(@RequestBody MonitorComponentSaveReq request) {
        monitorComponentService.save(request);
        return new Result<>();
    }

    @PutMapping
    @Operation(summary = "修改")
    @LogOperation("修改")
    @SaCheckPermission("ops:monitorcomponent:update")
    public Result<Object> update(@RequestBody MonitorComponentUpdateReq request) {
        monitorComponentService.update(request);
        return new Result<>();
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @SaCheckPermission("ops:monitorcomponent:delete")
    public Result<Object> delete(@RequestBody Long[] ids) {
        monitorComponentService.delete(MonitorComponentDeleteReq.of(ids));
        return new Result<>();
    }

    @GetMapping("check")
    @Operation(summary = "唯一校验")
    @SaCheckPermission("ops:monitorcomponent:page")
    public Result<Boolean> check(MonitorComponentCheckReq request) {
        return new Result<Boolean>().ok(monitorComponentService.check(request));
    }

    @GetMapping("probe")
    @Operation(summary = "探测")
    @SaCheckPermission("ops:monitorcomponent:probe")
    public Result<Boolean> probe(MonitorComponentProbeReq request) {
        return new Result<Boolean>().ok(monitorComponentService.probe(request));
    }

    @GetMapping("version")
    @Operation(summary = "版本检测")
    @SaCheckPermission("ops:monitorcomponent:version")
    public Result<MonitorComponentRsp> version(MonitorComponentVersionReq request) {
        return new Result<MonitorComponentRsp>().ok(monitorComponentService.versionCheck(request));
    }

}
