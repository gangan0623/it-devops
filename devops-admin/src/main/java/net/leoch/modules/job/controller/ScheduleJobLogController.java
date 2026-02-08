package net.leoch.modules.job.controller;

import cn.dev33.satoken.annotation.SaCheckPermission;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.AllArgsConstructor;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.Result;
import net.leoch.modules.job.dto.ScheduleJobLogDTO;
import net.leoch.modules.job.dto.ScheduleJobLogPageRequest;
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
@AllArgsConstructor
public class ScheduleJobLogController {
    private final IScheduleJobLogService scheduleJobLogService;

    @GetMapping("page")
    @Operation(summary = "分页")
    @SaCheckPermission("sys:schedule:log")
    public Result<PageData<ScheduleJobLogDTO>> page(ScheduleJobLogPageRequest request) {
        return new Result<PageData<ScheduleJobLogDTO>>().ok(scheduleJobLogService.page(request));
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @SaCheckPermission("sys:schedule:log")
    public Result<ScheduleJobLogDTO> info(@PathVariable("id") Long id) {
        ScheduleJobLogDTO log = scheduleJobLogService.get(id);

        return new Result<ScheduleJobLogDTO>().ok(log);
    }
}
