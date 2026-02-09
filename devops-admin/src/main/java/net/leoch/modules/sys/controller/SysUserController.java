package net.leoch.modules.sys.controller;

import cn.dev33.satoken.annotation.SaCheckPermission;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import net.leoch.common.core.annotation.LogOperation;
import net.leoch.common.data.page.PageData;
import net.leoch.common.utils.ExcelUtils;
import net.leoch.common.utils.Result;
import net.leoch.modules.sys.vo.req.PasswordReq;
import net.leoch.modules.sys.vo.rsp.SysUserRsp;
import net.leoch.modules.sys.vo.req.SysUserPageReq;
import net.leoch.modules.sys.vo.req.SysUserReq;
import net.leoch.common.excel.SysUserExcel;
import net.leoch.modules.sys.service.ISysUserService;
import org.springframework.web.bind.annotation.*;

/**
 * 用户管理
 *
 * @author Taohongqiang
 */
@RestController
@RequestMapping("/sys/user")
@Tag(name = "用户管理")
@RequiredArgsConstructor
public class SysUserController {
    private final ISysUserService sysUserService;

    @GetMapping("page")
    @Operation(summary = "分页")
    @SaCheckPermission("sys:user:page")
    public Result<PageData<SysUserRsp>> page(SysUserPageReq request) {
        return new Result<PageData<SysUserRsp>>().ok(sysUserService.page(request));
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @SaCheckPermission("sys:user:info")
    public Result<SysUserRsp> get(@PathVariable("id") Long id) {
        return new Result<SysUserRsp>().ok(sysUserService.getWithRoles(id));
    }

    @GetMapping("info")
    @Operation(summary = "登录用户信息")
    public Result<SysUserRsp> info() {
        return new Result<SysUserRsp>().ok(sysUserService.getCurrentUserInfo());
    }

    @PutMapping("password")
    @Operation(summary = "修改密码")
    @LogOperation("修改密码")
    public Result<Object> password(@RequestBody PasswordReq dto) {
        sysUserService.changePassword(dto);
        return new Result<>();
    }

    @PostMapping
    @Operation(summary = "保存")
    @LogOperation("保存")
    @SaCheckPermission("sys:user:save")
    public Result<Object> save(@RequestBody SysUserReq dto) {
        sysUserService.save(dto);
        return new Result<>();
    }

    @PutMapping
    @Operation(summary = "修改")
    @LogOperation("修改")
    @SaCheckPermission("sys:user:update")
    public Result<Object> update(@RequestBody SysUserReq dto) {
        sysUserService.update(dto);
        return new Result<>();
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @SaCheckPermission("sys:user:delete")
    public Result<Object> delete(@RequestBody Long[] ids) {
        sysUserService.delete(ids);
        return new Result<>();
    }

    @GetMapping("export")
    @Operation(summary = "导出")
    @LogOperation("导出")
    @SaCheckPermission("sys:user:export")
    public void export(SysUserPageReq request, HttpServletResponse response) throws Exception {
        ExcelUtils.exportExcelToTarget(response, null, "用户管理", sysUserService.list(request), SysUserExcel.class);
    }
}
