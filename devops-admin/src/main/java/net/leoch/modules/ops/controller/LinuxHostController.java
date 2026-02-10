package net.leoch.modules.ops.controller;


import net.leoch.common.base.Constant;
import cn.dev33.satoken.annotation.SaCheckPermission;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import net.leoch.common.annotation.LogOperation;
import net.leoch.common.data.page.PageData;
import net.leoch.common.data.result.Result;
import net.leoch.modules.ops.vo.req.*;
import net.leoch.modules.ops.vo.rsp.*;
import net.leoch.modules.ops.service.ILinuxHostService;
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
@RequiredArgsConstructor
public class LinuxHostController {
    private final ILinuxHostService linuxHostService;

    @GetMapping("page")
    @Operation(summary = "分页")
    @Parameters({
            @Parameter(name = Constant.PAGE, description = "当前页码，从1开始", in = ParameterIn.QUERY, required = true, ref = "int"),
            @Parameter(name = Constant.LIMIT, description = "每页显示记录数", in = ParameterIn.QUERY, required = true, ref = "int"),
            @Parameter(name = Constant.ORDER_FIELD, description = "排序字段", in = ParameterIn.QUERY, ref = "String"),
            @Parameter(name = Constant.ORDER, description = "排序方式，可选值(asc、desc)", in = ParameterIn.QUERY, ref = "String")
    })
    @SaCheckPermission("ops:linuxhost:page")
    public Result<PageData<LinuxHostRsp>> page(@Parameter(hidden = true) LinuxHostPageReq request) {
        return new Result<PageData<LinuxHostRsp>>().ok(linuxHostService.page(request));
    }

    @GetMapping("summary")
    @Operation(summary = "状态汇总")
    @SaCheckPermission("ops:linuxhost:page")
    public Result<OpsHostStatusSummaryRsp> summary(@Parameter(hidden = true) LinuxHostPageReq request) {
        return new Result<OpsHostStatusSummaryRsp>().ok(linuxHostService.summary(request));
    }

    @GetMapping("{id:\\d+}")
    @Operation(summary = "信息")
    @SaCheckPermission("ops:linuxhost:info")
    public Result<LinuxHostRsp> get(@PathVariable Long id) {
        return new Result<LinuxHostRsp>().ok(linuxHostService.get(LinuxHostIdReq.of(id)));
    }

    @PostMapping
    @Operation(summary = "保存")
    @LogOperation("保存")
    @SaCheckPermission("ops:linuxhost:save")
    public Result<Object> save(@RequestBody LinuxHostSaveReq request) {
        linuxHostService.save(request);
        return new Result<>();
    }

    @PutMapping
    @Operation(summary = "修改")
    @LogOperation("修改")
    @SaCheckPermission("ops:linuxhost:update")
    public Result<Object> update(@RequestBody LinuxHostUpdateReq request) {
        linuxHostService.update(request);
        return new Result<>();
    }

    @PutMapping("status")
    @Operation(summary = "批量状态更新")
    @LogOperation("批量状态更新")
    @SaCheckPermission("ops:linuxhost:update")
    public Result<Object> updateStatus(@RequestBody LinuxHostStatusUpdateReq request) {
        linuxHostService.updateStatus(request);
        return new Result<>();
    }

    @PostMapping("import")
    @Operation(summary = "导入")
    @LogOperation("导入")
    @SaCheckPermission("ops:linuxhost:import")
    public Result<Object> importExcel(@ModelAttribute LinuxHostImportReq request) throws Exception {
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
    public Result<Boolean> online(LinuxHostOnlineReq request) {
        return new Result<Boolean>().ok(linuxHostService.online(request));
    }

    @GetMapping("check")
    @Operation(summary = "唯一校验")
    @SaCheckPermission("ops:linuxhost:page")
    public Result<Boolean> check(LinuxHostCheckReq request) {
        return new Result<Boolean>().ok(linuxHostService.check(request));
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @SaCheckPermission("ops:linuxhost:delete")
    public Result<Object> delete(@RequestBody Long[] ids) {
        linuxHostService.delete(LinuxHostDeleteReq.of(ids));
        return new Result<>();
    }

    @GetMapping("export")
    @Operation(summary = "导出")
    @LogOperation("导出")
    @SaCheckPermission("ops:linuxhost:export")
    public void export(@Parameter(hidden = true) @ModelAttribute LinuxHostPageReq request, HttpServletResponse response) throws Exception {
        linuxHostService.export(request, response);
    }

}
