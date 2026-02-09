package net.leoch.modules.job.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.data.page.BasePage;

/**
 * 定时任务分页请求
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(name = "ScheduleJobPageReq")
public class ScheduleJobPageReq extends BasePage {

    @Schema(description = "beanName")
    private String beanName;
}
