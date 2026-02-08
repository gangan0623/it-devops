package net.leoch.modules.job.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.page.BasePage;

/**
 * 定时任务日志分页请求
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(name = "ScheduleJobLogPageRequest")
public class ScheduleJobLogPageRequest extends BasePage {

    @Schema(description = "任务ID")
    private String jobId;
}
