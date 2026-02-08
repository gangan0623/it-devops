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
import net.leoch.modules.sys.dto.SysRoleDTO;
import net.leoch.modules.sys.dto.SysRolePageRequest;
import net.leoch.modules.sys.service.ISysRoleDataScopeService;
import net.leoch.modules.sys.service.ISysRoleMenuService;
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
    private final ISysRoleMenuService sysRoleMenuService;
    private final ISysRoleDataScopeService sysRoleDataScopeService;

    @GetMapping("page")
    @Operation(summary = "分页")
    @SaCheckPermission("sys:role:page")
    public Result<PageData<SysRoleDTO>> page(SysRolePageRequest request) {
        PageData<SysRoleDTO> page = sysRoleService.page(request);

        return new Result<PageData<SysRoleDTO>>().ok(page);
    }

    @GetMapping("list")
    @Operation(summary = "列表")
    @SaCheckPermission("sys:role:list")
    public Result<List<SysRoleDTO>> list() {
        List<SysRoleDTO> data = sysRoleService.list(new SysRolePageRequest());

        return new Result<List<SysRoleDTO>>().ok(data);
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @SaCheckPermission("sys:role:info")
    public Result<SysRoleDTO> get(@PathVariable("id") Long id) {
        SysRoleDTO data = sysRoleService.get(id);

        //查询角色对应的菜单
        List<Long> menuIdList = sysRoleMenuService.getMenuIdList(id);
        data.setMenuIdList(menuIdList);

        //查询角色对应的数据权限
        List<Long> deptIdList = sysRoleDataScopeService.getDeptIdList(id);
        data.setDeptIdList(deptIdList);

        return new Result<SysRoleDTO>().ok(data);
    }

    @PostMapping
    @Operation(summary = "保存")
    @LogOperation("保存")
    @SaCheckPermission("sys:role:save")
    public Result<Object> save(@RequestBody SysRoleDTO dto) {
        //效验数据
        ValidatorUtils.validateEntity(dto, AddGroup.class, DefaultGroup.class);

        sysRoleService.save(dto);

        return new Result<>();
    }

    @PutMapping
    @Operation(summary = "修改")
    @LogOperation("修改")
    @SaCheckPermission("sys:role:update")
    public Result<Object> update(@RequestBody SysRoleDTO dto) {
        //效验数据
        ValidatorUtils.validateEntity(dto, UpdateGroup.class, DefaultGroup.class);

        sysRoleService.update(dto);

        return new Result<>();
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @SaCheckPermission("sys:role:delete")
    public Result<Object> delete(@RequestBody Long[] ids) {
        //效验数据
        AssertUtils.isArrayEmpty(ids, "id");

        sysRoleService.delete(ids);

        return new Result<>();
    }
}
