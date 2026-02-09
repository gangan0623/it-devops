package net.leoch.modules.sys.controller;

import cn.dev33.satoken.annotation.SaCheckPermission;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import net.leoch.common.annotation.LogOperation;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.Result;
import net.leoch.modules.sys.vo.rsp.SysDictTypeRsp;
import net.leoch.modules.sys.vo.req.SysDictTypePageReq;
import net.leoch.modules.sys.vo.req.SysDictTypeReq;
import net.leoch.modules.sys.vo.rsp.DictTypeRsp;
import net.leoch.modules.sys.service.ISysDictTypeService;
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
@RequiredArgsConstructor
public class SysDictTypeController {
    private final ISysDictTypeService sysDictTypeService;

    @GetMapping("page")
    @Operation(summary = "字典类型")
    @SaCheckPermission("sys:dict:page")
    public Result<PageData<SysDictTypeRsp>> page(SysDictTypePageReq request) {
        return new Result<PageData<SysDictTypeRsp>>().ok(sysDictTypeService.page(request));
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @SaCheckPermission("sys:dict:info")
    public Result<SysDictTypeRsp> get(@PathVariable("id") Long id) {
        return new Result<SysDictTypeRsp>().ok(sysDictTypeService.get(id));
    }

    @PostMapping
    @Operation(summary = "保存")
    @LogOperation("保存")
    @SaCheckPermission("sys:dict:save")
    public Result<Object> save(@RequestBody SysDictTypeReq dto) {
        sysDictTypeService.save(dto);
        return new Result<>();
    }

    @PutMapping
    @Operation(summary = "修改")
    @LogOperation("修改")
    @SaCheckPermission("sys:dict:update")
    public Result<Object> update(@RequestBody SysDictTypeReq dto) {
        sysDictTypeService.update(dto);
        return new Result<>();
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @SaCheckPermission("sys:dict:delete")
    public Result<Object> delete(@RequestBody Long[] ids) {
        sysDictTypeService.delete(ids);
        return new Result<>();
    }

    @GetMapping("all")
    @Operation(summary = "所有字典数据")
    public Result<List<DictTypeRsp>> all() {
        return new Result<List<DictTypeRsp>>().ok(sysDictTypeService.getAllList());
    }

}
