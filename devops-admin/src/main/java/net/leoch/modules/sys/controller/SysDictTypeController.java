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
import net.leoch.modules.sys.dto.SysDictTypeDTO;
import net.leoch.modules.sys.dto.SysDictTypePageRequest;
import net.leoch.modules.sys.entity.DictType;
import net.leoch.modules.sys.service.SysDictTypeService;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 字典类型
 *
 * @author Taohongqiang
 */
@RestController
@RequestMapping("sys/dict/type")
@Tag(name = "字典类型")
@AllArgsConstructor
public class SysDictTypeController {
    private final SysDictTypeService sysDictTypeService;

    @GetMapping("page")
    @Operation(summary = "字典类型")
    @SaCheckPermission("sys:dict:page")
    public Result<PageData<SysDictTypeDTO>> page(SysDictTypePageRequest request) {
        //字典类型
        PageData<SysDictTypeDTO> page = sysDictTypeService.page(request);

        return new Result<PageData<SysDictTypeDTO>>().ok(page);
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @SaCheckPermission("sys:dict:info")
    public Result<SysDictTypeDTO> get(@PathVariable("id") Long id) {
        SysDictTypeDTO data = sysDictTypeService.get(id);

        return new Result<SysDictTypeDTO>().ok(data);
    }

    @PostMapping
    @Operation(summary = "保存")
    @LogOperation("保存")
    @SaCheckPermission("sys:dict:save")
    public Result<Object> save(@RequestBody SysDictTypeDTO dto) {
        //效验数据
        ValidatorUtils.validateEntity(dto, DefaultGroup.class);

        sysDictTypeService.save(dto);

        return new Result<>();
    }

    @PutMapping
    @Operation(summary = "修改")
    @LogOperation("修改")
    @SaCheckPermission("sys:dict:update")
    public Result<Object> update(@RequestBody SysDictTypeDTO dto) {
        //效验数据
        ValidatorUtils.validateEntity(dto, UpdateGroup.class, DefaultGroup.class);

        sysDictTypeService.update(dto);

        return new Result<>();
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @SaCheckPermission("sys:dict:delete")
    public Result<Object> delete(@RequestBody Long[] ids) {
        //效验数据
        AssertUtils.isArrayEmpty(ids, "id");

        sysDictTypeService.delete(ids);

        return new Result<>();
    }

    @GetMapping("all")
    @Operation(summary = "所有字典数据")
    public Result<List<DictType>> all() {
        List<DictType> list = sysDictTypeService.getAllList();

        return new Result<List<DictType>>().ok(list);
    }

}
