package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 监控组件预览项
 */
@Data
@Schema(name = "DashboardMonitorComponentItem")
public class DashboardMonitorComponentItem implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "名称")
    private String name;

    @Schema(description = "在线状态 0离线 1在线")
    private Integer onlineStatus;

    @Schema(description = "是否可更新 0否 1是")
    private Integer updateAvailable;
}
