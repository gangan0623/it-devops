package net.leoch.modules.log.controller;

import cn.dev33.satoken.annotation.SaCheckPermission;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import net.leoch.common.core.annotation.LogOperation;
import net.leoch.common.data.page.PageData;
import net.leoch.common.utils.ExcelUtils;
import net.leoch.common.utils.Result;
import net.leoch.modules.log.vo.rsp.SysLogOperationRsp;
import net.leoch.modules.log.vo.req.SysLogOperationPageReq;
import net.leoch.common.excel.SysLogOperationExcel;
import net.leoch.modules.log.service.ISysLogOperationService;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


/**
 * 操作日志
 *
 * @author Taohongqiang
 * @since 1.0.0
 */
@RestController
@RequestMapping("sys/log/operation")
@Tag(name = "操作日志")
@RequiredArgsConstructor
public class SysLogOperationController {
    private final ISysLogOperationService sysLogOperationService;

    @GetMapping("page")
    @Operation(summary = "分页")
    @SaCheckPermission("sys:log:operation")
    public Result<PageData<SysLogOperationRsp>> page(SysLogOperationPageReq request) {
        return new Result<PageData<SysLogOperationRsp>>().ok(sysLogOperationService.page(request));
    }

    @GetMapping("export")
    @Operation(summary = "导出")
    @LogOperation("导出")
    @SaCheckPermission("sys:log:operation")
    public void export(SysLogOperationPageReq request, HttpServletResponse response) throws Exception {
        ExcelUtils.exportExcelToTarget(response, null, "操作日志", sysLogOperationService.list(request), SysLogOperationExcel.class);
    }

}
