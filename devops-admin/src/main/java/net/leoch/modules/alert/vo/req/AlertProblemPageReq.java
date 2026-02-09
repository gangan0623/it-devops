package net.leoch.modules.alert.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.page.BasePage;

/**
 * 告警问题分页请求
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(name = "AlertProblemPageReq")
public class AlertProblemPageReq extends BasePage {

    @Schema(description = "类别(realtime/history)")
    private String category;

    @Schema(description = "严重性(多个逗号分隔)")
    private String severity;

    @Schema(description = "设备类型")
    private String deviceType;

    @Schema(description = "主机名")
    private String hostName;

    @Schema(description = "实例")
    private String instance;

    @Schema(description = "确认状态")
    private String ackStatus;

    @Schema(description = "状态过滤(problem/manual/auto/resolved)")
    private String statusFilter;

    @Schema(description = "开始时间(yyyy-MM-dd HH:mm:ss)")
    private String startTime;

    @Schema(description = "结束时间(yyyy-MM-dd HH:mm:ss)")
    private String endTime;
}
