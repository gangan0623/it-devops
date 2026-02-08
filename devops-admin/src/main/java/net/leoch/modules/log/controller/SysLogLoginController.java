package net.leoch.modules.log.controller;

import cn.dev33.satoken.annotation.SaCheckPermission;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletResponse;
import lombok.AllArgsConstructor;
import net.leoch.common.annotation.LogOperation;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.ExcelUtils;
import net.leoch.common.utils.Result;
import net.leoch.modules.log.dto.SysLogLoginDTO;
import net.leoch.modules.log.dto.SysLogLoginPageRequest;
import net.leoch.modules.log.excel.SysLogLoginExcel;
import net.leoch.modules.log.service.SysLogLoginService;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;


/**
 * 登录日志
 *
 * @author Taohongqiang
 * @since 1.0.0
 */
@RestController
@RequestMapping("sys/log/login")
@Tag(name = "登录日志")
@AllArgsConstructor
public class SysLogLoginController {
    private final SysLogLoginService sysLogLoginService;

    @GetMapping("page")
    @Operation(summary = "分页")
    @SaCheckPermission("sys:log:login")
    public Result<PageData<SysLogLoginDTO>> page(SysLogLoginPageRequest request) {
        PageData<SysLogLoginDTO> page = sysLogLoginService.page(request);

        return new Result<PageData<SysLogLoginDTO>>().ok(page);
    }

    @GetMapping("export")
    @Operation(summary = "导出")
    @LogOperation("导出")
    @SaCheckPermission("sys:log:login")
    public void export(SysLogLoginPageRequest request, HttpServletResponse response) throws Exception {
        List<SysLogLoginDTO> list = sysLogLoginService.list(request);

        ExcelUtils.exportExcelToTarget(response, null, "登录日志", list, SysLogLoginExcel.class);

    }

}
