package net.leoch.modules.job.controller;

import cn.dev33.satoken.annotation.SaCheckPermission;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import net.leoch.common.data.page.PageData;
import net.leoch.common.support.utils.Result;
import net.leoch.modules.job.vo.rsp.ScheduleJobLogRsp;
import net.leoch.modules.job.vo.req.ScheduleJobLogPageReq;
import net.leoch.modules.job.service.IScheduleJobLogService;
import org.springframework.web.bind.annotation.*;

/**
 * 定时任务日志
 *
 * @author Taohongqiang
 */
@RestController
@RequestMapping("/sys/scheduleLog")
@Tag(name = "定时任务日志")
@RequiredArgsConstructor
public class ScheduleJobLogController {
    private final IScheduleJobLogService scheduleJobLogService;

    @GetMapping("page")
    @Operation(summary = "分页")
    @SaCheckPermission("sys:schedule:log")
    public Result<PageData<ScheduleJobLogRsp>> page(ScheduleJobLogPageReq request) {
        return new Result<PageData<ScheduleJobLogRsp>>().ok(scheduleJobLogService.page(request));
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @SaCheckPermission("sys:schedule:log")
    public Result<ScheduleJobLogRsp> info(@PathVariable("id") Long id) {
        return new Result<ScheduleJobLogRsp>().ok(scheduleJobLogService.get(id));
    }
}
