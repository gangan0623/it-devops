package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;
import java.util.Date;

/**
 * 实时告警摘要
 */
@Data
@Schema(name = "DashboardAlertSummary")
public class DashboardAlertSummary implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "告警名称")
    private String alertName;

    @Schema(description = "实例")
    private String instance;

    @Schema(description = "主机名")
    private String hostName;

    @Schema(description = "时间")
    private Date time;

    @Schema(description = "级别")
    private String severity;

    @Schema(description = "状态")
    private String status;
}
