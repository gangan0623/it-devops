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
import net.leoch.modules.ops.service.WindowHostService;
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
public class WindowHostController {
    
    @Resource
    private WindowHostService windowHostService;

    @GetMapping("page")
    @Operation(summary = "分页")
    @Parameters({
        @Parameter(name = Constant.PAGE, description = "当前页码，从1开始", in = ParameterIn.QUERY, required = true, ref="int") ,
        @Parameter(name = Constant.LIMIT, description = "每页显示记录数", in = ParameterIn.QUERY,required = true, ref="int") ,
        @Parameter(name = Constant.ORDER_FIELD, description = "排序字段", in = ParameterIn.QUERY, ref="String") ,
        @Parameter(name = Constant.ORDER, description = "排序方式，可选值(asc、desc)", in = ParameterIn.QUERY, ref="String")
    })
    @SaCheckPermission("ops:windowhost:page")
    public Result<PageData<WindowHostDTO>> page(@Parameter(hidden = true) WindowHostPageRequest request){
        return new Result<PageData<WindowHostDTO>>().ok(windowHostService.page(request));
    }

    @GetMapping("summary")
    @Operation(summary = "状态汇总")
    @SaCheckPermission("ops:windowhost:page")
    public Result<OpsHostStatusSummaryDTO> summary(@Parameter(hidden = true) WindowHostPageRequest request){
        return new Result<OpsHostStatusSummaryDTO>().ok(windowHostService.summary(request));
    }

    @GetMapping("{id:\\d+}")
    @Operation(summary = "信息")
    @SaCheckPermission("ops:windowhost:info")
    public Result<WindowHostDTO> get(@PathVariable Long id){
        return new Result<WindowHostDTO>().ok(windowHostService.get(WindowHostIdRequest.of(id)));
    }

    @PostMapping
    @Operation(summary = "保存")
    @LogOperation("保存")
    @SaCheckPermission("ops:windowhost:save")
    public Result<Object> save(@RequestBody WindowHostSaveRequest request){
        windowHostService.save(request);
        return new Result<>();
    }

    @PutMapping
    @Operation(summary = "修改")
    @LogOperation("修改")
    @SaCheckPermission("ops:windowhost:update")
    public Result<Object> update(@RequestBody WindowHostUpdateRequest request){
        windowHostService.update(request);
        return new Result<>();
    }

    @PutMapping("status")
    @Operation(summary = "批量状态更新")
    @LogOperation("批量状态更新")
    @SaCheckPermission("ops:windowhost:update")
    public Result<Object> updateStatus(@RequestBody WindowHostStatusUpdateRequest request){
        windowHostService.updateStatus(request);
        return new Result<>();
    }

    @PostMapping("import")
    @Operation(summary = "导入")
    @LogOperation("导入")
    @SaCheckPermission("ops:windowhost:import")
    public Result<Object> importExcel(@ModelAttribute WindowHostImportRequest request) throws Exception {
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
    public Result<Boolean> online(WindowHostOnlineRequest request){
        return new Result<Boolean>().ok(windowHostService.online(request));
    }

    @GetMapping("check")
    @Operation(summary = "唯一校验")
    @SaCheckPermission("ops:windowhost:page")
    public Result<Boolean> check(WindowHostCheckRequest request){
        return new Result<Boolean>().ok(windowHostService.check(request));
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @SaCheckPermission("ops:windowhost:delete")
    public Result<Object> delete(@RequestBody Long[] ids){
        WindowHostDeleteRequest request = new WindowHostDeleteRequest();
        request.setIds(ids);
        windowHostService.delete(request);
        return new Result<>();
    }

    @GetMapping("export")
    @Operation(summary = "导出")
    @LogOperation("导出")
    @SaCheckPermission("ops:windowhost:export")
    public void export(@Parameter(hidden = true) @ModelAttribute WindowHostPageRequest request, HttpServletResponse response) throws Exception {
        windowHostService.export(request, response);
    }

}
