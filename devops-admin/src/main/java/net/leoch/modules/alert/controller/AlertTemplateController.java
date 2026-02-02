package net.leoch.modules.alert.controller;

import cn.dev33.satoken.annotation.SaCheckPermission;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.tags.Tag;
import net.leoch.common.annotation.LogOperation;
import net.leoch.common.constant.Constant;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.Result;
import net.leoch.common.validator.AssertUtils;
import net.leoch.modules.alert.dto.AlertTemplateDTO;
import net.leoch.modules.alert.dto.AlertTemplatePreviewDTO;
import net.leoch.modules.alert.dto.AlertTemplateSendTestDTO;
import net.leoch.modules.alert.service.AlertTemplateService;
import net.leoch.modules.alert.service.AlertTriggerService;
import net.leoch.modules.alert.utils.AlertJsonUtils;
import net.leoch.modules.alert.utils.AlertPayloadUtils;
import net.leoch.modules.alert.utils.AlertTemplateRenderer;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.List;
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
public class AlertTemplateController {

    private final AlertTemplateService alertTemplateService;
    private final AlertTriggerService alertTriggerService;

    public AlertTemplateController(AlertTemplateService alertTemplateService, AlertTriggerService alertTriggerService) {
        this.alertTemplateService = alertTemplateService;
        this.alertTriggerService = alertTriggerService;
    }

    @GetMapping("page")
    @Operation(summary = "分页")
    @Parameters({
        @Parameter(name = Constant.PAGE, description = "当前页码，从1开始", in = ParameterIn.QUERY, required = true, ref="int") ,
        @Parameter(name = Constant.LIMIT, description = "每页显示记录数", in = ParameterIn.QUERY,required = true, ref="int") ,
        @Parameter(name = Constant.ORDER_FIELD, description = "排序字段", in = ParameterIn.QUERY, ref="String") ,
        @Parameter(name = Constant.ORDER, description = "排序方式，可选值(asc、desc)", in = ParameterIn.QUERY, ref="String")
    })
    @SaCheckPermission("alert:template:page")
    public Result<PageData<AlertTemplateDTO>> page(@Parameter(hidden = true) @RequestParam Map<String, Object> params){
        PageData<AlertTemplateDTO> page = alertTemplateService.page(params);

        return new Result<PageData<AlertTemplateDTO>>().ok(page);
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @SaCheckPermission("alert:template:info")
    public Result<AlertTemplateDTO> get(@PathVariable("id") Long id){
        AlertTemplateDTO data = alertTemplateService.get(id);

        return new Result<AlertTemplateDTO>().ok(data);
    }

    @PostMapping
    @Operation(summary = "保存")
    @LogOperation("保存")
    @SaCheckPermission("alert:template:save")
    public Result<Object> save(@RequestBody AlertTemplateDTO dto){
        alertTemplateService.save(dto);

        return new Result<>();
    }

    @PostMapping(consumes = {MediaType.APPLICATION_FORM_URLENCODED_VALUE, MediaType.MULTIPART_FORM_DATA_VALUE})
    @Operation(summary = "保存(表单)")
    @LogOperation("保存")
    @SaCheckPermission("alert:template:save")
    public Result<Object> saveForm(AlertTemplateDTO dto){
        alertTemplateService.save(dto);

        return new Result<>();
    }

    @PutMapping
    @Operation(summary = "修改")
    @LogOperation("修改")
    @SaCheckPermission("alert:template:update")
    public Result<Object> update(@RequestBody AlertTemplateDTO dto){
        alertTemplateService.update(dto);

        return new Result<>();
    }

    @PutMapping(consumes = {MediaType.APPLICATION_FORM_URLENCODED_VALUE, MediaType.MULTIPART_FORM_DATA_VALUE})
    @Operation(summary = "修改(表单)")
    @LogOperation("修改")
    @SaCheckPermission("alert:template:update")
    public Result<Object> updateForm(AlertTemplateDTO dto){
        alertTemplateService.update(dto);

        return new Result<>();
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @SaCheckPermission("alert:template:delete")
    public Result<Object> delete(@RequestBody Long[] ids){
        AssertUtils.isArrayEmpty(ids, "id");
        alertTemplateService.delete(ids);

        return new Result<>();
    }

    @PostMapping("preview")
    @Operation(summary = "模板预览")
    @SaCheckPermission("alert:template:test")
    public Result<Map<String, Object>> preview(@RequestBody AlertTemplatePreviewDTO dto) {
        if (dto == null || StrUtil.isBlank(dto.getRawJson())) {
            return new Result<Map<String, Object>>().error("原始JSON不能为空");
        }
        AlertTemplateDTO template = alertTemplateService.get(dto.getTemplateId());
        if (template == null) {
            return new Result<Map<String, Object>>().error("模板不存在");
        }
        Map<String, Object> payload = AlertJsonUtils.parsePayload(dto.getRawJson());
        List<Map<String, Object>> alerts = AlertPayloadUtils.getAlerts(payload);
        Map<String, Object> alert = CollUtil.isNotEmpty(alerts) ? alerts.get(0) : new HashMap<>();
        Map<String, Object> context = AlertPayloadUtils.buildContext(payload, alert, null);
        Map<String, Object> result = new HashMap<>();
        result.put("subject", AlertTemplateRenderer.render(template.getEmailSubject(), context));
        result.put("html", AlertTemplateRenderer.render(template.getEmailHtml(), context));

        return new Result<Map<String, Object>>().ok(result);
    }

    @PostMapping("test-send")
    @Operation(summary = "模板发送测试")
    @SaCheckPermission("alert:template:test")
    public Result<Object> testSend(@RequestBody AlertTemplateSendTestDTO dto) {
        alertTriggerService.sendTest(dto.getTemplateId(), dto.getTriggerId(), dto.getRawJson());
        return new Result<>();
    }
}
