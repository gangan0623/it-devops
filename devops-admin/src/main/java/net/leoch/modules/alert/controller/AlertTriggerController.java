package net.leoch.modules.alert.controller;

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
import net.leoch.modules.alert.dto.AlertMediaDTO;
import net.leoch.modules.alert.dto.AlertTemplateDTO;
import net.leoch.modules.alert.dto.AlertTriggerDTO;
import net.leoch.modules.alert.service.AlertMediaService;
import net.leoch.modules.alert.service.AlertTemplateService;
import net.leoch.modules.alert.service.AlertTriggerService;
import net.leoch.modules.sys.dao.SysUserDao;
import net.leoch.modules.sys.entity.SysUserEntity;
import org.apache.shiro.authz.annotation.RequiresPermissions;
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
    private final AlertTemplateService alertTemplateService;
    private final AlertMediaService alertMediaService;
    private final SysUserDao sysUserDao;

    public AlertTriggerController(AlertTriggerService alertTriggerService, AlertTemplateService alertTemplateService, AlertMediaService alertMediaService, SysUserDao sysUserDao) {
        this.alertTriggerService = alertTriggerService;
        this.alertTemplateService = alertTemplateService;
        this.alertMediaService = alertMediaService;
        this.sysUserDao = sysUserDao;
    }

    @GetMapping("page")
    @Operation(summary = "分页")
    @Parameters({
        @Parameter(name = Constant.PAGE, description = "当前页码，从1开始", in = ParameterIn.QUERY, required = true, ref="int") ,
        @Parameter(name = Constant.LIMIT, description = "每页显示记录数", in = ParameterIn.QUERY,required = true, ref="int") ,
        @Parameter(name = Constant.ORDER_FIELD, description = "排序字段", in = ParameterIn.QUERY, ref="String") ,
        @Parameter(name = Constant.ORDER, description = "排序方式，可选值(asc、desc)", in = ParameterIn.QUERY, ref="String")
    })
    @RequiresPermissions("alert:trigger:page")
    public Result<PageData<AlertTriggerDTO>> page(@Parameter(hidden = true) @RequestParam Map<String, Object> params){
        PageData<AlertTriggerDTO> page = alertTriggerService.page(params);
        alertTriggerService.fillReceiverUserIdList(page.getList());

        return new Result<PageData<AlertTriggerDTO>>().ok(page);
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @RequiresPermissions("alert:trigger:info")
    public Result<AlertTriggerDTO> get(@PathVariable("id") Long id){
        AlertTriggerDTO data = alertTriggerService.get(id);
        alertTriggerService.fillReceiverUserIdList(data);

        return new Result<AlertTriggerDTO>().ok(data);
    }

    @PostMapping
    @Operation(summary = "保存")
    @LogOperation("保存")
    @RequiresPermissions("alert:trigger:save")
    public Result<Object> save(@RequestBody AlertTriggerDTO dto){
        normalizeReceiverIds(dto);
        alertTriggerService.save(dto);

        return new Result<>();
    }

    @PutMapping
    @Operation(summary = "修改")
    @LogOperation("修改")
    @RequiresPermissions("alert:trigger:update")
    public Result<Object> update(@RequestBody AlertTriggerDTO dto){
        normalizeReceiverIds(dto);
        alertTriggerService.update(dto);

        return new Result<>();
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @RequiresPermissions("alert:trigger:delete")
    public Result<Object> delete(@RequestBody Long[] ids){
        AssertUtils.isArrayEmpty(ids, "id");
        alertTriggerService.delete(ids);

        return new Result<>();
    }

    @GetMapping("resources")
    @Operation(summary = "资源")
    @RequiresPermissions("alert:trigger:page")
    public Result<Map<String, Object>> resources() {
        Map<String, Object> result = new HashMap<>();
        List<AlertTemplateDTO> templates = alertTemplateService.list(new HashMap<>());
        List<AlertMediaDTO> medias = alertMediaService.list(new HashMap<>());
        List<SysUserEntity> users = sysUserDao.selectList(new com.baomidou.mybatisplus.core.conditions.query.QueryWrapper<SysUserEntity>()
            .select("id", "username", "email")
            .isNotNull("email")
        );
        result.put("templates", templates.stream().map(item -> {
            Map<String, Object> map = new HashMap<>();
            map.put("id", item.getId());
            map.put("name", item.getName());
            return map;
        }).collect(Collectors.toList()));
        result.put("medias", medias.stream().map(item -> {
            Map<String, Object> map = new HashMap<>();
            map.put("id", item.getId());
            map.put("name", item.getName());
            return map;
        }).collect(Collectors.toList()));
        result.put("users", users.stream().map(item -> {
            Map<String, Object> map = new HashMap<>();
            map.put("id", item.getId());
            map.put("name", item.getUsername());
            map.put("email", item.getEmail());
            return map;
        }).collect(Collectors.toList()));
        return new Result<Map<String, Object>>().ok(result);
    }

    @GetMapping("options")
    @Operation(summary = "触发器选项")
    @RequiresPermissions("alert:trigger:page")
    public Result<List<Map<String, Object>>> options() {
        List<AlertTriggerDTO> list = alertTriggerService.list(new HashMap<>());
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
