package net.leoch.modules.ops.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import net.leoch.common.utils.Result;
import net.leoch.modules.ops.vo.rsp.DashboardSummaryRsp;
import net.leoch.modules.ops.service.IDashboardService;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 工作台统计
 */
@RestController
@RequestMapping("ops/dashboard")
@Tag(name = "工作台统计")
public class DashboardController {

    private final IDashboardService dashboardService;

    public DashboardController(IDashboardService dashboardService) {
        this.dashboardService = dashboardService;
    }

    @GetMapping("summary")
    @Operation(summary = "统计概览")
    public Result<DashboardSummaryRsp> summary() {
        return new Result<DashboardSummaryRsp>().ok(dashboardService.summary());
    }
}
