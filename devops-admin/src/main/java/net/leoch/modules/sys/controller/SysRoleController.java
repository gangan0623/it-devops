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
import net.leoch.common.validator.group.AddGroup;
import net.leoch.common.validator.group.DefaultGroup;
import net.leoch.common.validator.group.UpdateGroup;
import net.leoch.modules.sys.vo.rsp.SysRoleRsp;
import net.leoch.modules.sys.vo.req.SysRolePageReq;
import net.leoch.modules.sys.vo.req.SysRoleReq;
import net.leoch.modules.sys.service.ISysRoleService;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 角色管理
 *
 * @author Taohongqiang
 */
@RestController
@RequestMapping("/sys/role")
@Tag(name = "角色管理")
@AllArgsConstructor
public class SysRoleController {
    private final ISysRoleService sysRoleService;

    @GetMapping("page")
    @Operation(summary = "分页")
    @SaCheckPermission("sys:role:page")
    public Result<PageData<SysRoleRsp>> page(SysRolePageReq request) {
        return new Result<PageData<SysRoleRsp>>().ok(sysRoleService.page(request));
    }

    @GetMapping("list")
    @Operation(summary = "列表")
    @SaCheckPermission("sys:role:list")
    public Result<List<SysRoleRsp>> list() {
        return new Result<List<SysRoleRsp>>().ok(sysRoleService.list(new SysRolePageReq()));
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @SaCheckPermission("sys:role:info")
    public Result<SysRoleRsp> get(@PathVariable("id") Long id) {
        return new Result<SysRoleRsp>().ok(sysRoleService.getWithMenuAndDataScope(id));
    }

    @PostMapping
    @Operation(summary = "保存")
    @LogOperation("保存")
    @SaCheckPermission("sys:role:save")
    public Result<Object> save(@RequestBody SysRoleReq dto) {
        ValidatorUtils.validateEntity(dto, AddGroup.class, DefaultGroup.class);
        sysRoleService.save(dto);
        return new Result<>();
    }

    @PutMapping
    @Operation(summary = "修改")
    @LogOperation("修改")
    @SaCheckPermission("sys:role:update")
    public Result<Object> update(@RequestBody SysRoleReq dto) {
        ValidatorUtils.validateEntity(dto, UpdateGroup.class, DefaultGroup.class);
        sysRoleService.update(dto);
        return new Result<>();
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @SaCheckPermission("sys:role:delete")
    public Result<Object> delete(@RequestBody Long[] ids) {
        AssertUtils.isArrayEmpty(ids, "id");
        sysRoleService.delete(ids);
        return new Result<>();
    }
}
