package net.leoch.modules.sys.controller;

import cn.dev33.satoken.annotation.SaCheckPermission;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.AllArgsConstructor;
import net.leoch.common.annotation.LogOperation;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.Result;
import net.leoch.common.validator.AssertUtils;
import net.leoch.common.validator.ValidatorUtils;
import net.leoch.common.validator.group.DefaultGroup;
import net.leoch.common.validator.group.UpdateGroup;
import net.leoch.modules.sys.vo.rsp.SysDictDataRsp;
import net.leoch.modules.sys.vo.req.SysDictDataPageReq;
import net.leoch.modules.sys.vo.req.SysDictDataReq;
import net.leoch.modules.sys.service.ISysDictDataService;
import org.springframework.web.bind.annotation.*;

/**
 * 字典数据
 *
 * @author Taohongqiang
 */
@RestController
@RequestMapping("sys/dict/data")
@Tag(name = "字典数据")
@AllArgsConstructor
public class SysDictDataController {
    private final ISysDictDataService sysDictDataService;

    @GetMapping("page")
    @Operation(summary = "字典数据")
    @SaCheckPermission("sys:dict:page")
    public Result<PageData<SysDictDataRsp>> page(SysDictDataPageReq request) {
        return new Result<PageData<SysDictDataRsp>>().ok(sysDictDataService.page(request));
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @SaCheckPermission("sys:dict:info")
    public Result<SysDictDataRsp> get(@PathVariable("id") Long id) {
        return new Result<SysDictDataRsp>().ok(sysDictDataService.get(id));
    }

    @PostMapping
    @Operation(summary = "保存")
    @LogOperation("保存")
    @SaCheckPermission("sys:dict:save")
    public Result<Object> save(@RequestBody SysDictDataReq dto) {
        ValidatorUtils.validateEntity(dto, DefaultGroup.class);
        sysDictDataService.save(dto);
        return new Result<>();
    }

    @PutMapping
    @Operation(summary = "修改")
    @LogOperation("修改")
    @SaCheckPermission("sys:dict:update")
    public Result<Object> update(@RequestBody SysDictDataReq dto) {
        ValidatorUtils.validateEntity(dto, UpdateGroup.class, DefaultGroup.class);
        sysDictDataService.update(dto);
        return new Result<>();
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @SaCheckPermission("sys:dict:delete")
    public Result<Object> delete(@RequestBody Long[] ids) {
        AssertUtils.isArrayEmpty(ids, "id");
        sysDictDataService.delete(ids);
        return new Result<>();
    }

}
