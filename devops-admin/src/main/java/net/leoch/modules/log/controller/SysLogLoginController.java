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
import net.leoch.modules.log.vo.rsp.SysLogLoginRsp;
import net.leoch.modules.log.vo.req.SysLogLoginPageReq;
import net.leoch.modules.log.excel.SysLogLoginExcel;
import net.leoch.modules.log.service.ISysLogLoginService;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


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
    private final ISysLogLoginService sysLogLoginService;

    @GetMapping("page")
    @Operation(summary = "分页")
    @SaCheckPermission("sys:log:login")
    public Result<PageData<SysLogLoginRsp>> page(SysLogLoginPageReq request) {
        return new Result<PageData<SysLogLoginRsp>>().ok(sysLogLoginService.page(request));
    }

    @GetMapping("export")
    @Operation(summary = "导出")
    @LogOperation("导出")
    @SaCheckPermission("sys:log:login")
    public void export(SysLogLoginPageReq request, HttpServletResponse response) throws Exception {
        ExcelUtils.exportExcelToTarget(response, null, "登录日志", sysLogLoginService.list(request), SysLogLoginExcel.class);
    }

}
