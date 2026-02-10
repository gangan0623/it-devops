package net.leoch.modules.alert.controller;


import net.leoch.common.base.Constant;
import cn.dev33.satoken.annotation.SaCheckPermission;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import net.leoch.common.annotation.LogOperation;
import net.leoch.common.data.page.PageData;
import net.leoch.common.data.result.Result;
import net.leoch.modules.alert.vo.rsp.AlertTemplateRsp;
import net.leoch.modules.alert.vo.req.AlertTemplatePageReq;
import net.leoch.modules.alert.vo.req.AlertTemplateReq;
import net.leoch.modules.alert.vo.req.AlertTemplatePreviewReq;
import net.leoch.modules.alert.vo.req.AlertTemplateSendTestReq;
import net.leoch.modules.alert.service.IAlertTemplateService;
import net.leoch.modules.alert.service.IAlertTriggerService;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * 告警模板
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@RestController
@RequestMapping("alert/template")
@Tag(name = "告警模板")
@RequiredArgsConstructor
public class AlertTemplateController {

    private final IAlertTemplateService alertTemplateService;
    private final IAlertTriggerService alertTriggerService;

    @GetMapping("page")
    @Operation(summary = "分页")
    @Parameters({
        @Parameter(name = Constant.PAGE, description = "当前页码，从1开始", in = ParameterIn.QUERY, required = true, ref="int") ,
        @Parameter(name = Constant.LIMIT, description = "每页显示记录数", in = ParameterIn.QUERY,required = true, ref="int") ,
        @Parameter(name = Constant.ORDER_FIELD, description = "排序字段", in = ParameterIn.QUERY, ref="String") ,
        @Parameter(name = Constant.ORDER, description = "排序方式，可选值(asc、desc)", in = ParameterIn.QUERY, ref="String")
    })
    @SaCheckPermission("alert:template:page")
    public Result<PageData<AlertTemplateRsp>> page(AlertTemplatePageReq request){
        return new Result<PageData<AlertTemplateRsp>>().ok(alertTemplateService.page(request));
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @SaCheckPermission("alert:template:info")
    public Result<AlertTemplateRsp> get(@PathVariable("id") Long id){
        return new Result<AlertTemplateRsp>().ok(alertTemplateService.get(id));
    }

    @PostMapping
    @Operation(summary = "保存")
    @LogOperation("保存")
    @SaCheckPermission("alert:template:save")
    public Result<Object> save(@RequestBody AlertTemplateReq dto){
        alertTemplateService.save(dto);
        return new Result<>();
    }

    @PostMapping(consumes = {MediaType.APPLICATION_FORM_URLENCODED_VALUE, MediaType.MULTIPART_FORM_DATA_VALUE})
    @Operation(summary = "保存(表单)")
    @LogOperation("保存")
    @SaCheckPermission("alert:template:save")
    public Result<Object> saveForm(AlertTemplateReq dto){
        alertTemplateService.save(dto);
        return new Result<>();
    }

    @PutMapping
    @Operation(summary = "修改")
    @LogOperation("修改")
    @SaCheckPermission("alert:template:update")
    public Result<Object> update(@RequestBody AlertTemplateReq dto){
        alertTemplateService.update(dto);
        return new Result<>();
    }

    @PutMapping(consumes = {MediaType.APPLICATION_FORM_URLENCODED_VALUE, MediaType.MULTIPART_FORM_DATA_VALUE})
    @Operation(summary = "修改(表单)")
    @LogOperation("修改")
    @SaCheckPermission("alert:template:update")
    public Result<Object> updateForm(AlertTemplateReq dto){
        alertTemplateService.update(dto);
        return new Result<>();
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @SaCheckPermission("alert:template:delete")
    public Result<Object> delete(@RequestBody Long[] ids){
        alertTemplateService.delete(ids);
        return new Result<>();
    }

    @PostMapping("preview")
    @Operation(summary = "模板预览")
    @SaCheckPermission("alert:template:test")
    public Result<Map<String, Object>> preview(@RequestBody AlertTemplatePreviewReq dto) {
        return new Result<Map<String, Object>>().ok(alertTemplateService.preview(dto));
    }

    @PostMapping("test-send")
    @Operation(summary = "模板发送测试")
    @SaCheckPermission("alert:template:test")
    public Result<Object> testSend(@RequestBody AlertTemplateSendTestReq dto) {
        alertTriggerService.sendTest(dto.getTemplateId(), dto.getTriggerId(), dto.getRawJson());
        return new Result<>();
    }
}
