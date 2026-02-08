package net.leoch.modules.ops.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;
import java.util.List;

/**
 * 工作台统计响应
 */
@Data
@Schema(name = "DashboardSummaryRsp")
public class DashboardSummaryRsp implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "主机数量统计")
    private DashboardHostCountsRsp hostCounts;

    @Schema(description = "备份统计")
    private DashboardBackupStatsRsp backupStats;

    @Schema(description = "设备差异")
    private DashboardDeviceDiffRsp deviceDiff;

    @Schema(description = "实时告警")
    private List<DashboardAlertSummaryRsp> recentAlerts;

    @Schema(description = "监控组件预览")
    private List<DashboardMonitorComponentItemRsp> monitorComponents;
}
