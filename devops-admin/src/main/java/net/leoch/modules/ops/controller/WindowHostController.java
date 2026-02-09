package net.leoch.modules.ops.controller;


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
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import net.leoch.common.annotation.LogOperation;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.Result;
import net.leoch.modules.ops.vo.req.*;
import net.leoch.modules.ops.vo.rsp.*;
import net.leoch.modules.ops.service.IWindowHostService;
import org.springframework.web.bind.annotation.*;


/**
 * Windows主机表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@RestController
@RequestMapping("ops/windowhost")
@Tag(name="Windows主机表")
@RequiredArgsConstructor
public class WindowHostController {
    private final IWindowHostService windowHostService;

    @GetMapping("page")
    @Operation(summary = "分页")
    @Parameters({
        @Parameter(name = Constant.PAGE, description = "当前页码，从1开始", in = ParameterIn.QUERY, required = true, ref="int") ,
        @Parameter(name = Constant.LIMIT, description = "每页显示记录数", in = ParameterIn.QUERY,required = true, ref="int") ,
        @Parameter(name = Constant.ORDER_FIELD, description = "排序字段", in = ParameterIn.QUERY, ref="String") ,
        @Parameter(name = Constant.ORDER, description = "排序方式，可选值(asc、desc)", in = ParameterIn.QUERY, ref="String")
    })
    @SaCheckPermission("ops:windowhost:page")
    public Result<PageData<WindowHostRsp>> page(@Parameter(hidden = true) WindowHostPageReq request){
        return new Result<PageData<WindowHostRsp>>().ok(windowHostService.page(request));
    }

    @GetMapping("summary")
    @Operation(summary = "状态汇总")
    @SaCheckPermission("ops:windowhost:page")
    public Result<OpsHostStatusSummaryRsp> summary(@Parameter(hidden = true) WindowHostPageReq request){
        return new Result<OpsHostStatusSummaryRsp>().ok(windowHostService.summary(request));
    }

    @GetMapping("{id:\\d+}")
    @Operation(summary = "信息")
    @SaCheckPermission("ops:windowhost:info")
    public Result<WindowHostRsp> get(@PathVariable Long id){
        return new Result<WindowHostRsp>().ok(windowHostService.get(WindowHostIdReq.of(id)));
    }

    @PostMapping
    @Operation(summary = "保存")
    @LogOperation("保存")
    @SaCheckPermission("ops:windowhost:save")
    public Result<Object> save(@RequestBody WindowHostSaveReq request){
        windowHostService.save(request);
        return new Result<>();
    }

    @PutMapping
    @Operation(summary = "修改")
    @LogOperation("修改")
    @SaCheckPermission("ops:windowhost:update")
    public Result<Object> update(@RequestBody WindowHostUpdateReq request){
        windowHostService.update(request);
        return new Result<>();
    }

    @PutMapping("status")
    @Operation(summary = "批量状态更新")
    @LogOperation("批量状态更新")
    @SaCheckPermission("ops:windowhost:update")
    public Result<Object> updateStatus(@RequestBody WindowHostStatusUpdateReq request){
        windowHostService.updateStatus(request);
        return new Result<>();
    }

    @PostMapping("import")
    @Operation(summary = "导入")
    @LogOperation("导入")
    @SaCheckPermission("ops:windowhost:import")
    public Result<Object> importExcel(@ModelAttribute WindowHostImportReq request) throws Exception {
        windowHostService.importExcel(request);
        return new Result<>();
    }

    @GetMapping("template")
    @Operation(summary = "导入模板")
    @LogOperation("导入模板")
    @SaCheckPermission("ops:windowhost:template")
    public void template(HttpServletResponse response) throws Exception {
        windowHostService.template(response);
    }

    @GetMapping("online")
    @Operation(summary = "在线状态")
    @SaCheckPermission("ops:windowhost:page")
    public Result<Boolean> online(WindowHostOnlineReq request){
        return new Result<Boolean>().ok(windowHostService.online(request));
    }

    @GetMapping("check")
    @Operation(summary = "唯一校验")
    @SaCheckPermission("ops:windowhost:page")
    public Result<Boolean> check(WindowHostCheckReq request){
        return new Result<Boolean>().ok(windowHostService.check(request));
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @SaCheckPermission("ops:windowhost:delete")
    public Result<Object> delete(@RequestBody Long[] ids){
        windowHostService.delete(WindowHostDeleteReq.of(ids));
        return new Result<>();
    }

    @GetMapping("export")
    @Operation(summary = "导出")
    @LogOperation("导出")
    @SaCheckPermission("ops:windowhost:export")
    public void export(@Parameter(hidden = true) @ModelAttribute WindowHostPageReq request, HttpServletResponse response) throws Exception {
        windowHostService.export(request, response);
    }

}
