package net.leoch.modules.log.controller;

import cn.dev33.satoken.annotation.SaCheckPermission;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import net.leoch.common.annotation.LogOperation;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.ExcelUtils;
import net.leoch.common.utils.Result;
import net.leoch.modules.log.vo.rsp.SysLogErrorRsp;
import net.leoch.modules.log.vo.req.SysLogErrorPageReq;
import net.leoch.modules.log.excel.SysLogErrorExcel;
import net.leoch.modules.log.service.ISysLogErrorService;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


/**
 * 异常日志
 *
 * @author Taohongqiang
 * @since 1.0.0
 */
@RestController
@RequestMapping("sys/log/error")
@Tag(name = "异常日志")
@RequiredArgsConstructor
public class SysLogErrorController {
    private final ISysLogErrorService sysLogErrorService;

    @GetMapping("page")
    @Operation(summary = "分页")
    @SaCheckPermission("sys:log:error")
    public Result<PageData<SysLogErrorRsp>> page(SysLogErrorPageReq request) {
        return new Result<PageData<SysLogErrorRsp>>().ok(sysLogErrorService.page(request));
    }

    @GetMapping("export")
    @Operation(summary = "导出")
    @LogOperation("导出")
    @SaCheckPermission("sys:log:error")
    public void export(SysLogErrorPageReq request, HttpServletResponse response) throws Exception {
        ExcelUtils.exportExcelToTarget(response, null, "异常日志", sysLogErrorService.list(request), SysLogErrorExcel.class);
    }

}
