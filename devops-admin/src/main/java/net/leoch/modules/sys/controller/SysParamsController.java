package net.leoch.modules.sys.controller;

import cn.dev33.satoken.annotation.SaCheckPermission;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import net.leoch.common.core.annotation.LogOperation;
import net.leoch.common.data.page.PageData;
import net.leoch.common.utils.ExcelUtils;
import net.leoch.common.utils.Result;
import net.leoch.modules.sys.vo.rsp.SysParamsRsp;
import net.leoch.modules.sys.vo.req.SysParamsPageReq;
import net.leoch.modules.sys.vo.req.SysParamsReq;
import net.leoch.common.integration.excel.SysParamsExcel;
import net.leoch.modules.sys.service.ISysParamsService;
import org.springframework.web.bind.annotation.*;


/**
 * 参数管理
 *
 * @author Taohongqiang
 * @since 1.0.0
 */
@RestController
@RequestMapping("sys/params")
@Tag(name = "参数管理")
@RequiredArgsConstructor
public class SysParamsController {
    private final ISysParamsService sysParamsService;

    @GetMapping("page")
    @Operation(summary = "分页")
    @SaCheckPermission("sys:params:page")
    public Result<PageData<SysParamsRsp>> page(SysParamsPageReq request) {
        return new Result<PageData<SysParamsRsp>>().ok(sysParamsService.page(request));
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @SaCheckPermission("sys:params:info")
    public Result<SysParamsRsp> get(@PathVariable("id") Long id) {
        return new Result<SysParamsRsp>().ok(sysParamsService.get(id));
    }

    @PostMapping
    @Operation(summary = "保存")
    @LogOperation("保存")
    @SaCheckPermission("sys:params:save")
    public Result<Object> save(@RequestBody SysParamsReq dto) {
        sysParamsService.save(dto);
        return new Result<>();
    }

    @PutMapping
    @Operation(summary = "修改")
    @LogOperation("修改")
    @SaCheckPermission("sys:params:update")
    public Result<Object> update(@RequestBody SysParamsReq dto) {
        sysParamsService.update(dto);
        return new Result<>();
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @SaCheckPermission("sys:params:delete")
    public Result<Object> delete(@RequestBody Long[] ids) {
        sysParamsService.delete(ids);
        return new Result<>();
    }

    @GetMapping("export")
    @Operation(summary = "导出")
    @LogOperation("导出")
    @SaCheckPermission("sys:params:export")
    public void export(SysParamsPageReq request, HttpServletResponse response) throws Exception {
        ExcelUtils.exportExcelToTarget(response, null, "参数管理", sysParamsService.list(request), SysParamsExcel.class);
    }

}
