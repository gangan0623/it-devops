package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;
import java.util.List;

/**
 * 工作台统计响应
 */
@Data
@Schema(name = "DashboardSummaryResponse")
public class DashboardSummaryResponse implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "主机数量统计")
    private DashboardHostCounts hostCounts;

    @Schema(description = "备份统计")
    private DashboardBackupStats backupStats;

    @Schema(description = "设备差异")
    private DashboardDeviceDiff deviceDiff;

    @Schema(description = "实时告警")
    private List<DashboardAlertSummary> recentAlerts;

    @Schema(description = "监控组件预览")
    private List<DashboardMonitorComponentItem> monitorComponents;
}
