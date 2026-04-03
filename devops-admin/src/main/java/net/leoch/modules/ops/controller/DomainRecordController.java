package net.leoch.modules.ops.controller;

import cn.dev33.satoken.annotation.SaCheckPermission;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import net.leoch.common.annotation.LogOperation;
import net.leoch.common.base.Constant;
import net.leoch.common.data.page.PageData;
import net.leoch.common.data.result.Result;
import net.leoch.modules.ops.service.IDomainRecordService;
import net.leoch.modules.ops.vo.req.DomainRecordDeleteReq;
import net.leoch.modules.ops.vo.req.DomainRecordIdReq;
import net.leoch.modules.ops.vo.req.DomainRecordImportReq;
import net.leoch.modules.ops.vo.req.DomainRecordPageReq;
import net.leoch.modules.ops.vo.req.DomainRecordSaveReq;
import net.leoch.modules.ops.vo.req.DomainRecordUpdateReq;
import net.leoch.modules.ops.vo.rsp.DomainRecordDetailRsp;
import net.leoch.modules.ops.vo.rsp.OpsHostStatusSummaryRsp;
import net.leoch.modules.ops.vo.rsp.DomainRecordRsp;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

@RestController
@RequestMapping("ops/domain-record")
@Tag(name = "域名记录")
@RequiredArgsConstructor
public class DomainRecordController {

    private final IDomainRecordService domainRecordService;

    @GetMapping("page")
    @Operation(summary = "分页")
    @Parameters({
            @Parameter(name = Constant.PAGE, description = "当前页码，从1开始", in = ParameterIn.QUERY, required = true, ref = "int"),
            @Parameter(name = Constant.LIMIT, description = "每页显示记录数", in = ParameterIn.QUERY, required = true, ref = "int"),
            @Parameter(name = Constant.ORDER_FIELD, description = "排序字段", in = ParameterIn.QUERY, ref = "String"),
            @Parameter(name = Constant.ORDER, description = "排序方式，可选值(asc、desc)", in = ParameterIn.QUERY, ref = "String")
    })
    @SaCheckPermission("ops:domain-record:page")
    public Result<PageData<DomainRecordRsp>> page(@Parameter(hidden = true) DomainRecordPageReq request) {
        return new Result<PageData<DomainRecordRsp>>().ok(domainRecordService.page(request));
    }

    @GetMapping("summary")
    @Operation(summary = "状态汇总")
    @SaCheckPermission("ops:domain-record:page")
    public Result<OpsHostStatusSummaryRsp> summary(@Parameter(hidden = true) DomainRecordPageReq request) {
        return new Result<OpsHostStatusSummaryRsp>().ok(domainRecordService.summary(request));
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @SaCheckPermission("ops:domain-record:info")
    public Result<DomainRecordDetailRsp> get(@PathVariable Long id) {
        return new Result<DomainRecordDetailRsp>().ok(domainRecordService.get(DomainRecordIdReq.of(id)));
    }

    @PostMapping
    @Operation(summary = "保存")
    @LogOperation("保存域名记录")
    @SaCheckPermission("ops:domain-record:save")
    public Result<Object> save(@RequestBody DomainRecordSaveReq request) {
        domainRecordService.save(request);
        return new Result<>();
    }

    @PutMapping
    @Operation(summary = "修改")
    @LogOperation("修改域名记录")
    @SaCheckPermission("ops:domain-record:update")
    public Result<Object> update(@RequestBody DomainRecordUpdateReq request) {
        domainRecordService.update(request);
        return new Result<>();
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除域名记录")
    @SaCheckPermission("ops:domain-record:delete")
    public Result<Object> delete(@Valid @RequestBody DomainRecordDeleteReq request) {
        domainRecordService.delete(request);
        return new Result<>();
    }

    @PostMapping("import")
    @Operation(summary = "导入")
    @LogOperation("导入域名记录")
    @SaCheckPermission("ops:domain-record:save")
    public Result<Object> importExcel(@RequestParam("file") MultipartFile file) throws Exception {
        DomainRecordImportReq request = new DomainRecordImportReq();
        request.setFile(file);
        domainRecordService.importExcel(request);
        return new Result<>();
    }

    @GetMapping("template")
    @Operation(summary = "模板下载")
    @SaCheckPermission("ops:domain-record:page")
    public void template(HttpServletResponse response) throws Exception {
        domainRecordService.template(response);
    }

    @GetMapping("export")
    @Operation(summary = "导出")
    @SaCheckPermission("ops:domain-record:page")
    public void export(DomainRecordPageReq request, HttpServletResponse response) throws Exception {
        domainRecordService.export(request, response);
    }
}
