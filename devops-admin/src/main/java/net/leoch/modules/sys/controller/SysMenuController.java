

package net.leoch.modules.sys.controller;

import cn.dev33.satoken.annotation.SaCheckPermission;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import net.leoch.common.annotation.LogOperation;
import net.leoch.common.utils.Result;
import net.leoch.common.enums.MenuTypeEnum;
import net.leoch.modules.sys.vo.rsp.SysMenuRsp;
import net.leoch.modules.sys.vo.req.SysMenuReq;
import net.leoch.modules.sys.service.ISysMenuService;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Set;

/**
 * 菜单管理
 *
 * @author Taohongqiang
 */
@RestController
@RequestMapping("/sys/menu")
@Tag(name = "菜单管理")
@RequiredArgsConstructor
public class SysMenuController {
    private final ISysMenuService sysMenuService;

    @GetMapping("nav")
    @Operation(summary = "导航")
    public Result<List<SysMenuRsp>> nav() {
        return new Result<List<SysMenuRsp>>().ok(sysMenuService.getCurrentUserMenuList(MenuTypeEnum.MENU.value()));
    }

    @GetMapping("permissions")
    @Operation(summary = "权限标识")
    public Result<Set<String>> permissions() {
        return new Result<Set<String>>().ok(sysMenuService.getCurrentUserPermissions());
    }

    @GetMapping("list")
    @Operation(summary = "列表")
    @Parameter(name = "type", description = "菜单类型 0：菜单 1：按钮  null：全部", in = ParameterIn.QUERY, ref = "int")
    @SaCheckPermission("sys:menu:list")
    public Result<List<SysMenuRsp>> list(Integer type) {
        return new Result<List<SysMenuRsp>>().ok(sysMenuService.getAllMenuList(type));
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @SaCheckPermission("sys:menu:info")
    public Result<SysMenuRsp> get(@PathVariable("id") Long id) {
        return new Result<SysMenuRsp>().ok(sysMenuService.get(id));
    }

    @PostMapping
    @Operation(summary = "保存")
    @LogOperation("保存")
    @SaCheckPermission("sys:menu:save")
    public Result<Object> save(@RequestBody SysMenuReq dto) {
        sysMenuService.save(dto);
        return new Result<>();
    }

    @PutMapping
    @Operation(summary = "修改")
    @LogOperation("修改")
    @SaCheckPermission("sys:menu:update")
    public Result<Object> update(@RequestBody SysMenuReq dto) {
        sysMenuService.update(dto);
        return new Result<>();
    }

    @DeleteMapping("{id}")
    @Operation(summary = "删除")
    @LogOperation("删除")
    @SaCheckPermission("sys:menu:delete")
    public Result<Object> delete(@PathVariable("id") Long id) {
        sysMenuService.deleteWithChildCheck(id);
        return new Result<>();
    }

    @GetMapping("select")
    @Operation(summary = "角色菜单权限")
    @SaCheckPermission("sys:menu:select")
    public Result<List<SysMenuRsp>> select() {
        return new Result<List<SysMenuRsp>>().ok(sysMenuService.getCurrentUserMenuList(null));
    }
}
