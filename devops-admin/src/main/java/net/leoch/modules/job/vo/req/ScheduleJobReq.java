package net.leoch.modules.job.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Null;
import lombok.Data;
import net.leoch.common.validator.group.AddGroup;
import net.leoch.common.validator.group.DefaultGroup;
import net.leoch.common.validator.group.UpdateGroup;
import org.hibernate.validator.constraints.Range;

import java.io.Serial;
import java.io.Serializable;

/**
 * 定时任务请求
 *
 * @author Taohongqiang
 */
@Data
@Schema(title = "定时任务请求")
public class ScheduleJobReq implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(title = "id")
    @Null(message = "{id.null}", groups = AddGroup.class)
    @NotNull(message = "{id.require}", groups = UpdateGroup.class)
    private Long id;

    @Schema(title = "spring bean名称")
    @NotBlank(message = "{schedule.bean.require}", groups = DefaultGroup.class)
    private String beanName;

    @Schema(title = "参数")
    private String params;

    @Schema(title = "cron表达式")
    @NotBlank(message = "{schedule.cron.require}", groups = DefaultGroup.class)
    private String cronExpression;

    @Schema(title = "任务状态  0：暂停  1：正常")
    @Range(min = 0, max = 1, message = "{schedule.status.range}", groups = DefaultGroup.class)
    private Integer status;

    @Schema(title = "备注")
    private String remark;
}
