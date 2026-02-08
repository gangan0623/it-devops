package net.leoch.modules.alert.controller;

import cn.dev33.satoken.annotation.SaCheckPermission;
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
import net.leoch.modules.alert.dto.AlertTriggerDTO;
import net.leoch.modules.alert.dto.AlertTriggerPageRequest;
import net.leoch.modules.alert.service.AlertTriggerService;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 告警触发器
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@RestController
@RequestMapping("alert/trigger")
@Tag(name = "告警触发器")
public class AlertTriggerController {

    private final AlertTriggerService alertTriggerService;

    public AlertTriggerController(AlertTriggerService alertTriggerService) {
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
    @SaCheckPermission("alert:trigger:page")
    public Result<PageData<AlertTriggerDTO>> page(AlertTriggerPageRequest request){
        PageData<AlertTriggerDTO> page = alertTriggerService.page(request);
        alertTriggerService.fillReceiverUserIdList(page.getList());

        return new Result<PageData<AlertTriggerDTO>>().ok(page);
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @SaCheckPermission("alert:trigger:info")
    public Result<AlertTriggerDTO> get(@PathVariable("id") Long id){
        AlertTriggerDTO data = alertTriggerService.get(id);
        alertTriggerService.fillReceiverUserIdList(data);

        return new Result<AlertTriggerDTO>().ok(data);
    }

    @PostMapping
    @Operation(summary = "保存")
    @LogOperation("保存")
    @SaCheckPermission("alert:trigger:save")
    public Result<Object> save(@RequestBody AlertTriggerDTO dto){
        normalizeReceiverIds(dto);
        alertTriggerService.save(dto);

        return new Result<>();
    }

    @PutMapping
    @Operation(summary = "修改")
    @LogOperation("修改")
    @SaCheckPermission("alert:trigger:update")
    public Result<Object> update(@RequestBody AlertTriggerDTO dto){
        normalizeReceiverIds(dto);
        alertTriggerService.update(dto);

        return new Result<>();
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @SaCheckPermission("alert:trigger:delete")
    public Result<Object> delete(@RequestBody Long[] ids){
        AssertUtils.isArrayEmpty(ids, "id");
        alertTriggerService.delete(ids);

        return new Result<>();
    }

    @GetMapping("resources")
    @Operation(summary = "资源")
    @SaCheckPermission("alert:trigger:page")
    public Result<Map<String, Object>> resources() {
        return new Result<Map<String, Object>>().ok(alertTriggerService.resources());
    }

    @GetMapping("options")
    @Operation(summary = "触发器选项")
    @SaCheckPermission("alert:trigger:page")
    public Result<List<Map<String, Object>>> options() {
        List<AlertTriggerDTO> list = alertTriggerService.list(new AlertTriggerPageRequest());
        List<Map<String, Object>> options = list.stream().map(item -> {
            Map<String, Object> map = new HashMap<>();
            map.put("id", item.getId());
            map.put("name", item.getName());
            return map;
        }).collect(Collectors.toList());
        return new Result<List<Map<String, Object>>>().ok(options);
    }

    private void normalizeReceiverIds(AlertTriggerDTO dto) {
        if (dto == null || dto.getReceiverUserIdList() == null) {
            return;
        }
        String joined = dto.getReceiverUserIdList().stream()
            .filter(id -> id != null)
            .map(String::valueOf)
            .collect(Collectors.joining(","));
        dto.setReceiverUserIds(StrUtil.isNotBlank(joined) ? joined : null);
    }
}
