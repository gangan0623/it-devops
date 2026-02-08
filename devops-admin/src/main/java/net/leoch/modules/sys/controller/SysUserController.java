package net.leoch.modules.sys.controller;

import cn.dev33.satoken.annotation.SaCheckPermission;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletResponse;
import lombok.AllArgsConstructor;
import net.leoch.common.annotation.LogOperation;
import net.leoch.common.exception.ErrorCode;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.ConvertUtils;
import net.leoch.common.utils.ExcelUtils;
import net.leoch.common.utils.Result;
import net.leoch.common.validator.AssertUtils;
import net.leoch.common.validator.ValidatorUtils;
import net.leoch.common.validator.group.AddGroup;
import net.leoch.common.validator.group.DefaultGroup;
import net.leoch.common.validator.group.UpdateGroup;
import net.leoch.modules.security.password.PasswordUtils;
import net.leoch.modules.security.user.SecurityUser;
import net.leoch.modules.security.user.UserDetail;
import net.leoch.modules.sys.dto.PasswordDTO;
import net.leoch.modules.sys.dto.SysUserDTO;
import net.leoch.modules.sys.dto.SysUserPageRequest;
import net.leoch.modules.sys.excel.SysUserExcel;
import net.leoch.modules.sys.service.ISysRoleUserService;
import net.leoch.modules.sys.service.ISysUserService;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 用户管理
 *
 * @author Taohongqiang
 */
@RestController
@RequestMapping("/sys/user")
@Tag(name = "用户管理")
@AllArgsConstructor
public class SysUserController {
    private final ISysUserService sysUserService;
    private final ISysRoleUserService sysRoleUserService;

    @GetMapping("page")
    @Operation(summary = "分页")
    @SaCheckPermission("sys:user:page")
    public Result<PageData<SysUserDTO>> page(SysUserPageRequest request) {
        PageData<SysUserDTO> page = sysUserService.page(request);

        return new Result<PageData<SysUserDTO>>().ok(page);
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @SaCheckPermission("sys:user:info")
    public Result<SysUserDTO> get(@PathVariable("id") Long id) {
        SysUserDTO data = sysUserService.get(id);

        //用户角色列表
        List<Long> roleIdList = sysRoleUserService.getRoleIdList(id);
        data.setRoleIdList(roleIdList);

        return new Result<SysUserDTO>().ok(data);
    }

    @GetMapping("info")
    @Operation(summary = "登录用户信息")
    public Result<SysUserDTO> info() {
        SysUserDTO data = ConvertUtils.sourceToTarget(SecurityUser.getUser(), SysUserDTO.class);
        return new Result<SysUserDTO>().ok(data);
    }

    @PutMapping("password")
    @Operation(summary = "修改密码")
    @LogOperation("修改密码")
    public Result<Object> password(@RequestBody PasswordDTO dto) {
        //效验数据
        ValidatorUtils.validateEntity(dto);

        UserDetail user = SecurityUser.getUser();

        //原密码不正确
        if (!PasswordUtils.matches(dto.getPassword(), user.getPassword())) {
            return new Result().error(ErrorCode.PASSWORD_ERROR);
        }

        sysUserService.updatePassword(user.getId(), dto.getNewPassword());

        return new Result<>();
    }

    @PostMapping
    @Operation(summary = "保存")
    @LogOperation("保存")
    @SaCheckPermission("sys:user:save")
    public Result<Object> save(@RequestBody SysUserDTO dto) {
        //效验数据
        ValidatorUtils.validateEntity(dto, AddGroup.class, DefaultGroup.class);

        sysUserService.save(dto);

        return new Result<>();
    }

    @PutMapping
    @Operation(summary = "修改")
    @LogOperation("修改")
    @SaCheckPermission("sys:user:update")
    public Result<Object> update(@RequestBody SysUserDTO dto) {
        //效验数据
        ValidatorUtils.validateEntity(dto, UpdateGroup.class, DefaultGroup.class);

        sysUserService.update(dto);

        return new Result<>();
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @SaCheckPermission("sys:user:delete")
    public Result<Object> delete(@RequestBody Long[] ids) {
        //效验数据
        AssertUtils.isArrayEmpty(ids, "id");

        sysUserService.delete(ids);

        return new Result<>();
    }

    @GetMapping("export")
    @Operation(summary = "导出")
    @LogOperation("导出")
    @SaCheckPermission("sys:user:export")
    public void export(SysUserPageRequest request, HttpServletResponse response) throws Exception {
        List<SysUserDTO> list = sysUserService.list(request);

        ExcelUtils.exportExcelToTarget(response, null, "用户管理", list, SysUserExcel.class);
    }
}
