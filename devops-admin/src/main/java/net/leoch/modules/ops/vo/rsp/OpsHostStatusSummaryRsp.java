package net.leoch.modules.ops.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 主机状态汇总
 */
@Data
@Schema(name = "OpsHostStatusSummaryRsp")
public class OpsHostStatusSummaryRsp implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "总数")
    private Long totalCount = 0L;

    @Schema(description = "启用数")
    private Long enabledCount = 0L;

    @Schema(description = "禁用数")
    private Long disabledCount = 0L;

    @Schema(description = "在线数")
    private Long onlineCount = 0L;

    @Schema(description = "离线数")
    private Long offlineCount = 0L;

    @Schema(description = "未知在线状态数")
    private Long unknownCount = 0L;
}
