package net.leoch.modules.job.controller;

import cn.dev33.satoken.annotation.SaCheckPermission;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.AllArgsConstructor;
import net.leoch.common.annotation.LogOperation;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.Result;
import net.leoch.common.validator.ValidatorUtils;
import net.leoch.common.validator.group.AddGroup;
import net.leoch.common.validator.group.DefaultGroup;
import net.leoch.common.validator.group.UpdateGroup;
import net.leoch.modules.job.vo.rsp.ScheduleJobRsp;
import net.leoch.modules.job.vo.req.ScheduleJobPageReq;
import net.leoch.modules.job.service.IScheduleJobService;
import org.springframework.web.bind.annotation.*;

/**
 * 定时任务
 *
 * @author Taohongqiang
 */
@RestController
@RequestMapping("/sys/schedule")
@Tag(name = "定时任务")
@AllArgsConstructor
public class ScheduleJobController {
    private final IScheduleJobService scheduleJobService;

    @GetMapping("page")
    @Operation(summary = "分页")
    @SaCheckPermission("sys:schedule:page")
    public Result<PageData<ScheduleJobRsp>> page(ScheduleJobPageReq request) {
        PageData<ScheduleJobRsp> page = scheduleJobService.page(request);

        return new Result<PageData<ScheduleJobRsp>>().ok(page);
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @SaCheckPermission("sys:schedule:info")
    public Result<ScheduleJobRsp> info(@PathVariable("id") Long id) {
        ScheduleJobRsp schedule = scheduleJobService.get(id);

        return new Result<ScheduleJobRsp>().ok(schedule);
    }

    @PostMapping
    @Operation(summary = "保存")
    @LogOperation("保存")
    @SaCheckPermission("sys:schedule:save")
    public Result<Object> save(@RequestBody ScheduleJobRsp dto) {
        ValidatorUtils.validateEntity(dto, AddGroup.class, DefaultGroup.class);

        scheduleJobService.save(dto);

        return new Result<>();
    }

    @PutMapping
    @Operation(summary = "修改")
    @LogOperation("修改")
    @SaCheckPermission("sys:schedule:update")
    public Result<Object> update(@RequestBody ScheduleJobRsp dto) {
        ValidatorUtils.validateEntity(dto, UpdateGroup.class, DefaultGroup.class);

        scheduleJobService.update(dto);

        return new Result<>();
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @SaCheckPermission("sys:schedule:delete")
    public Result<Object> delete(@RequestBody Long[] ids) {
        scheduleJobService.deleteBatch(ids);

        return new Result<>();
    }

    @PutMapping("/run")
    @Operation(summary = "立即执行")
    @LogOperation("立即执行")
    @SaCheckPermission("sys:schedule:run")
    public Result<Object> run(@RequestBody Long[] ids) {
        scheduleJobService.run(ids);

        return new Result<>();
    }

    @PutMapping("/pause")
    @Operation(summary = "暂停")
    @LogOperation("暂停")
    @SaCheckPermission("sys:schedule:pause")
    public Result<Object> pause(@RequestBody Long[] ids) {
        scheduleJobService.pause(ids);

        return new Result<>();
    }

    @PutMapping("/resume")
    @Operation(summary = "恢复")
    @LogOperation("恢复")
    @SaCheckPermission("sys:schedule:resume")
    public Result<Object> resume(@RequestBody Long[] ids) {
        scheduleJobService.resume(ids);

        return new Result<>();
    }

}
