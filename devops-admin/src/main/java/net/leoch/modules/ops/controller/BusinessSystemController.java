package net.leoch.modules.ops.controller;


import cn.dev33.satoken.annotation.SaCheckPermission;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import net.leoch.common.annotation.LogOperation;
import net.leoch.common.base.Constant;
import net.leoch.common.data.page.PageData;
import net.leoch.common.data.result.Result;
import net.leoch.modules.ops.service.IBusinessSystemService;
import net.leoch.modules.ops.vo.req.*;
import net.leoch.modules.ops.vo.rsp.BusinessSystemRsp;
import net.leoch.modules.ops.vo.rsp.OpsHostStatusSummaryRsp;
import org.springframework.web.bind.annotation.*;

/**
 * 业务系统表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@RestController
@RequestMapping("ops/businesssystem")
@Tag(name="业务系统表")
@RequiredArgsConstructor
public class BusinessSystemController {
    private final IBusinessSystemService businessSystemService;

    @GetMapping("page")
    @Operation(summary = "分页")
    @Parameters({
        @Parameter(name = Constant.PAGE, description = "当前页码，从1开始", in = ParameterIn.QUERY, required = true, ref="int") ,
        @Parameter(name = Constant.LIMIT, description = "每页显示记录数", in = ParameterIn.QUERY,required = true, ref="int") ,
        @Parameter(name = Constant.ORDER_FIELD, description = "排序字段", in = ParameterIn.QUERY, ref="String") ,
        @Parameter(name = Constant.ORDER, description = "排序方式，可选值(asc、desc)", in = ParameterIn.QUERY, ref="String")
    })
    @SaCheckPermission("ops:businesssystem:page")
    public Result<PageData<BusinessSystemRsp>> page(@Parameter(hidden = true) BusinessSystemPageReq request){
        return new Result<PageData<BusinessSystemRsp>>().ok(businessSystemService.page(request));
    }

    @GetMapping("summary")
    @Operation(summary = "状态汇总")
    @SaCheckPermission("ops:businesssystem:page")
    public Result<OpsHostStatusSummaryRsp> summary(@Parameter(hidden = true) BusinessSystemPageReq request){
        return new Result<OpsHostStatusSummaryRsp>().ok(businessSystemService.summary(request));
    }

    @GetMapping("{id:\\d+}")
    @Operation(summary = "信息")
    @SaCheckPermission("ops:businesssystem:info")
    public Result<BusinessSystemRsp> get(@PathVariable("id") Long id){
        return new Result<BusinessSystemRsp>().ok(businessSystemService.get(BusinessSystemIdReq.of(id)));
    }

    @PostMapping
    @Operation(summary = "保存")
    @LogOperation("保存")
    @SaCheckPermission("ops:businesssystem:save")
    public Result<Object> save(@RequestBody BusinessSystemSaveReq request){
        businessSystemService.save(request);
        return new Result<>();
    }

    @PutMapping
    @Operation(summary = "修改")
    @LogOperation("修改")
    @SaCheckPermission("ops:businesssystem:update")
    public Result<Object> update(@RequestBody BusinessSystemUpdateReq request){
        businessSystemService.update(request);
        return new Result<>();
    }

    @PutMapping("status")
    @Operation(summary = "批量状态更新")
    @LogOperation("批量状态更新")
    @SaCheckPermission("ops:businesssystem:update")
    public Result<Object> updateStatus(@RequestBody BusinessSystemStatusUpdateReq request){
        businessSystemService.updateStatus(request);
        return new Result<>();
    }

    @PostMapping("import")
    @Operation(summary = "导入")
    @LogOperation("导入")
    @SaCheckPermission("ops:businesssystem:import")
    public Result<Object> importExcel(@ModelAttribute BusinessSystemImportReq request) throws Exception {
        businessSystemService.importExcel(request);
        return new Result<>();
    }

    @GetMapping("template")
    @Operation(summary = "导入模板")
    @LogOperation("导入模板")
    @SaCheckPermission("ops:businesssystem:template")
    public void template(HttpServletResponse response) throws Exception {
        businessSystemService.template(response);
    }

    @GetMapping("online")
    @Operation(summary = "在线状态")
    @SaCheckPermission("ops:businesssystem:page")
    public Result<Boolean> online(BusinessSystemOnlineReq request){
        return new Result<Boolean>().ok(businessSystemService.online(request));
    }

    @GetMapping("check")
    @Operation(summary = "唯一校验")
    @SaCheckPermission("ops:businesssystem:page")
    public Result<Boolean> check(BusinessSystemCheckReq request){
        return new Result<Boolean>().ok(businessSystemService.check(request));
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @SaCheckPermission("ops:businesssystem:delete")
    public Result<Object> delete(@RequestBody Long[] ids){
        businessSystemService.delete(BusinessSystemDeleteReq.of(ids));
        return new Result<>();
    }

    @GetMapping("export")
    @Operation(summary = "导出")
    @LogOperation("导出")
    @SaCheckPermission("ops:businesssystem:export")
    public void export(@Parameter(hidden = true) @ModelAttribute BusinessSystemPageReq request, HttpServletResponse response) throws Exception {
        businessSystemService.export(request, response);
    }

}
