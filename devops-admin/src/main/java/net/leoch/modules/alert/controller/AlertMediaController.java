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
import net.leoch.modules.alert.dto.AlertMediaDTO;
import net.leoch.modules.alert.dto.AlertMediaPageRequest;
import net.leoch.modules.alert.dto.AlertMediaTestDTO;
import net.leoch.modules.alert.entity.AlertMediaEntity;
import net.leoch.modules.alert.service.AlertMailService;
import net.leoch.modules.alert.service.AlertMediaService;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 告警媒介
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@RestController
@RequestMapping("alert/media")
@Tag(name = "告警媒介")
public class AlertMediaController {

    private final AlertMediaService alertMediaService;
    private final AlertMailService alertMailService;

    public AlertMediaController(AlertMediaService alertMediaService, AlertMailService alertMailService) {
        this.alertMediaService = alertMediaService;
        this.alertMailService = alertMailService;
    }

    @GetMapping("page")
    @Operation(summary = "分页")
    @Parameters({
        @Parameter(name = Constant.PAGE, description = "当前页码，从1开始", in = ParameterIn.QUERY, required = true, ref="int") ,
        @Parameter(name = Constant.LIMIT, description = "每页显示记录数", in = ParameterIn.QUERY,required = true, ref="int") ,
        @Parameter(name = Constant.ORDER_FIELD, description = "排序字段", in = ParameterIn.QUERY, ref="String") ,
        @Parameter(name = Constant.ORDER, description = "排序方式，可选值(asc、desc)", in = ParameterIn.QUERY, ref="String")
    })
    @SaCheckPermission("alert:media:page")
    public Result<PageData<AlertMediaDTO>> page(AlertMediaPageRequest request){
        return new Result<PageData<AlertMediaDTO>>().ok(alertMediaService.page(request));
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @SaCheckPermission("alert:media:info")
    public Result<AlertMediaDTO> get(@PathVariable("id") Long id){
        AlertMediaDTO data = alertMediaService.get(id);

        return new Result<AlertMediaDTO>().ok(data);
    }

    @PostMapping
    @Operation(summary = "保存")
    @LogOperation("保存")
    @SaCheckPermission("alert:media:save")
    public Result<Object> save(@RequestBody AlertMediaDTO dto){
        alertMediaService.save(dto);

        return new Result<>();
    }

    @PutMapping
    @Operation(summary = "修改")
    @LogOperation("修改")
    @SaCheckPermission("alert:media:update")
    public Result<Object> update(@RequestBody AlertMediaDTO dto){
        alertMediaService.update(dto);

        return new Result<>();
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @SaCheckPermission("alert:media:delete")
    public Result<Object> delete(@RequestBody Long[] ids){
        AssertUtils.isArrayEmpty(ids, "id");
        alertMediaService.delete(ids);

        return new Result<>();
    }

    @PostMapping("test")
    @Operation(summary = "媒介测试")
    @SaCheckPermission("alert:media:test")
    public Result<Object> test(@RequestBody AlertMediaTestDTO dto) {
        if (StrUtil.isBlank(dto.getTo())) {
            return new Result().error("收件人不能为空");
        }
        AlertMediaDTO mediaDTO = alertMediaService.get(dto.getMediaId());
        if (mediaDTO == null) {
            return new Result().error("媒介不存在");
        }
        AlertMediaEntity media = new AlertMediaEntity();
        media.setHost(mediaDTO.getHost());
        media.setPort(mediaDTO.getPort());
        media.setUsername(mediaDTO.getUsername());
        media.setPassword(mediaDTO.getPassword());
        media.setProtocol(mediaDTO.getProtocol());
        media.setSmtpAuth(mediaDTO.getSmtpAuth());
        media.setStarttlsEnable(mediaDTO.getStarttlsEnable());
        media.setTlsEnable(mediaDTO.getTlsEnable());
        media.setFromAddr(mediaDTO.getFromAddr());

        List<String> receivers = StrUtil.split(dto.getTo(), ',').stream()
            .map(String::trim)
            .filter(StrUtil::isNotBlank)
            .collect(Collectors.toList());
        alertMailService.send(media, receivers, dto.getSubject(), dto.getContent(), dto.getHtml());
        return new Result<>();
    }
}
