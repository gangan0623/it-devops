package net.leoch.modules.alert.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;
import java.util.Date;

/**
 * 实时告警摘要
 */
@Data
@Schema(name = "AlertRealtimeDTO")
public class AlertRealtimeDTO implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "告警名称")
    private String alertName;

    @Schema(description = "实例")
    private String instance;

    @Schema(description = "级别")
    private String severity;

    @Schema(description = "状态")
    private String status;

    @Schema(description = "时间")
    private Date time;
}
