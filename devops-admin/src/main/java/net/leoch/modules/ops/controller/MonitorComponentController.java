package net.leoch.modules.ops.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import net.leoch.common.annotation.LogOperation;
import net.leoch.common.constant.Constant;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.Result;
import net.leoch.modules.ops.dto.*;
import net.leoch.modules.ops.service.MonitorComponentService;
import org.apache.shiro.authz.annotation.RequiresPermissions;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 监控组件
 */
@RestController
@RequestMapping("ops/monitorcomponent")
@Tag(name = "监控组件")
public class MonitorComponentController {

    @Resource
    private MonitorComponentService monitorComponentService;

    @GetMapping("page")
    @Operation(summary = "分页")
    @Parameters({
        @Parameter(name = Constant.PAGE, description = "当前页码，从1开始", in = ParameterIn.QUERY, required = true, ref="int"),
        @Parameter(name = Constant.LIMIT, description = "每页显示记录数", in = ParameterIn.QUERY, required = true, ref="int"),
        @Parameter(name = Constant.ORDER_FIELD, description = "排序字段", in = ParameterIn.QUERY, ref="String"),
        @Parameter(name = Constant.ORDER, description = "排序方式，可选值(asc、desc)", in = ParameterIn.QUERY, ref="String")
    })
    @RequiresPermissions("ops:monitorcomponent:page")
    public Result<PageData<MonitorComponentDTO>> page(@Parameter(hidden = true) MonitorComponentPageRequest request) {
        return new Result<PageData<MonitorComponentDTO>>().ok(monitorComponentService.page(request));
    }

    @GetMapping("list")
    @Operation(summary = "列表")
    @RequiresPermissions("ops:monitorcomponent:page")
    public Result<List<MonitorComponentDTO>> list(MonitorComponentListRequest request) {
        return new Result<List<MonitorComponentDTO>>().ok(monitorComponentService.list(request));
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @RequiresPermissions("ops:monitorcomponent:info")
    public Result<MonitorComponentDTO> get(@PathVariable("id") Long id) {
        return new Result<MonitorComponentDTO>().ok(monitorComponentService.get(MonitorComponentIdRequest.of(id)));
    }

    @PostMapping
    @Operation(summary = "保存")
    @LogOperation("保存")
    @RequiresPermissions("ops:monitorcomponent:save")
    public Result<Object> save(@RequestBody MonitorComponentSaveRequest request) {
        monitorComponentService.save(request);
        return new Result<>();
    }

    @PutMapping
    @Operation(summary = "修改")
    @LogOperation("修改")
    @RequiresPermissions("ops:monitorcomponent:update")
    public Result<Object> update(@RequestBody MonitorComponentUpdateRequest request) {
        monitorComponentService.update(request);
        return new Result<>();
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @RequiresPermissions("ops:monitorcomponent:delete")
    public Result<Object> delete(@RequestBody Long[] ids) {
        MonitorComponentDeleteRequest request = new MonitorComponentDeleteRequest();
        request.setIds(ids);
        monitorComponentService.delete(request);
        return new Result<>();
    }

    @GetMapping("check")
    @Operation(summary = "唯一校验")
    @RequiresPermissions("ops:monitorcomponent:page")
    public Result<Boolean> check(MonitorComponentCheckRequest request) {
        return new Result<Boolean>().ok(monitorComponentService.check(request));
    }

    @GetMapping("probe")
    @Operation(summary = "探测")
    @RequiresPermissions("ops:monitorcomponent:probe")
    public Result<Boolean> probe(MonitorComponentProbeRequest request) {
        return new Result<Boolean>().ok(monitorComponentService.probe(request));
    }

    @GetMapping("version")
    @Operation(summary = "版本检测")
    @RequiresPermissions("ops:monitorcomponent:version")
    public Result<MonitorComponentDTO> version(MonitorComponentVersionRequest request) {
        return new Result<MonitorComponentDTO>().ok(monitorComponentService.versionCheck(request));
    }

}
