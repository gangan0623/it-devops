package net.leoch.modules.ops.controller;

import cn.dev33.satoken.annotation.SaCheckPermission;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import jakarta.servlet.http.HttpServletResponse;
import net.leoch.common.annotation.LogOperation;
import net.leoch.common.constant.Constant;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.Result;
import net.leoch.modules.ops.dto.*;
import net.leoch.modules.ops.service.LinuxHostService;
import org.springframework.web.bind.annotation.*;

/**
 * Linux主机表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@RestController
@RequestMapping("ops/linuxhost")
@Tag(name = "Linux主机表")
public class LinuxHostController {
    @Resource
    private LinuxHostService linuxHostService;

    @GetMapping("page")
    @Operation(summary = "分页")
    @Parameters({
            @Parameter(name = Constant.PAGE, description = "当前页码，从1开始", in = ParameterIn.QUERY, required = true, ref = "int"),
            @Parameter(name = Constant.LIMIT, description = "每页显示记录数", in = ParameterIn.QUERY, required = true, ref = "int"),
            @Parameter(name = Constant.ORDER_FIELD, description = "排序字段", in = ParameterIn.QUERY, ref = "String"),
            @Parameter(name = Constant.ORDER, description = "排序方式，可选值(asc、desc)", in = ParameterIn.QUERY, ref = "String")
    })
    @SaCheckPermission("ops:linuxhost:page")
    public Result<PageData<LinuxHostDTO>> page(@Parameter(hidden = true) LinuxHostPageRequest request) {
        return new Result<PageData<LinuxHostDTO>>().ok(linuxHostService.page(request));
    }

    @GetMapping("summary")
    @Operation(summary = "状态汇总")
    @SaCheckPermission("ops:linuxhost:page")
    public Result<OpsHostStatusSummaryDTO> summary(@Parameter(hidden = true) LinuxHostPageRequest request) {
        return new Result<OpsHostStatusSummaryDTO>().ok(linuxHostService.summary(request));
    }

    @GetMapping("{id:\\d+}")
    @Operation(summary = "信息")
    @SaCheckPermission("ops:linuxhost:info")
    public Result<LinuxHostDTO> get(@PathVariable Long id) {
        return new Result<LinuxHostDTO>().ok(linuxHostService.get(LinuxHostIdRequest.of(id)));
    }

    @PostMapping
    @Operation(summary = "保存")
    @LogOperation("保存")
    @SaCheckPermission("ops:linuxhost:save")
    public Result<Object> save(@RequestBody LinuxHostSaveRequest request) {
        linuxHostService.save(request);
        return new Result<>();
    }

    @PutMapping
    @Operation(summary = "修改")
    @LogOperation("修改")
    @SaCheckPermission("ops:linuxhost:update")
    public Result<Object> update(@RequestBody LinuxHostUpdateRequest request) {
        linuxHostService.update(request);
        return new Result<>();
    }

    @PutMapping("status")
    @Operation(summary = "批量状态更新")
    @LogOperation("批量状态更新")
    @SaCheckPermission("ops:linuxhost:update")
    public Result<Object> updateStatus(@RequestBody LinuxHostStatusUpdateRequest request) {
        linuxHostService.updateStatus(request);
        return new Result<>();
    }

    @PostMapping("import")
    @Operation(summary = "导入")
    @LogOperation("导入")
    @SaCheckPermission("ops:linuxhost:import")
    public Result<Object> importExcel(@ModelAttribute LinuxHostImportRequest request) throws Exception {
        linuxHostService.importExcel(request);
        return new Result<>();
    }

    @GetMapping("template")
    @Operation(summary = "导入模板")
    @LogOperation("导入模板")
    @SaCheckPermission("ops:linuxhost:template")
    public void template(HttpServletResponse response) throws Exception {
        linuxHostService.template(response);
    }

    @GetMapping("online")
    @Operation(summary = "在线状态")
    @SaCheckPermission("ops:linuxhost:page")
    public Result<Boolean> online(LinuxHostOnlineRequest request) {
        return new Result<Boolean>().ok(linuxHostService.online(request));
    }

    @GetMapping("check")
    @Operation(summary = "唯一校验")
    @SaCheckPermission("ops:linuxhost:page")
    public Result<Boolean> check(LinuxHostCheckRequest request) {
        return new Result<Boolean>().ok(linuxHostService.check(request));
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @SaCheckPermission("ops:linuxhost:delete")
    public Result<Object> delete(@RequestBody Long[] ids) {
        LinuxHostDeleteRequest request = new LinuxHostDeleteRequest();
        request.setIds(ids);
        linuxHostService.delete(request);
        return new Result<>();
    }

    @GetMapping("export")
    @Operation(summary = "导出")
    @LogOperation("导出")
    @SaCheckPermission("ops:linuxhost:export")
    public void export(@Parameter(hidden = true) @ModelAttribute LinuxHostPageRequest request, HttpServletResponse response) throws Exception {
        linuxHostService.export(request, response);
    }

}
