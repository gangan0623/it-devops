package net.leoch.modules.alert.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.Date;

@Data
@Schema(title = "Prometheus告警AI报告响应")
public class AlertAiReportPrometheusRsp implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "报告类型 server/http")
    private String reportType;

    @Schema(description = "周期类型")
    private String periodType;

    @Schema(description = "周期开始")
    private Date periodStart;

    @Schema(description = "周期结束")
    private Date periodEnd;

    @Schema(description = "模型")
    private String modelName;

    @Schema(description = "状态 0处理中 1成功 2失败")
    private Integer reportStatus;

    @Schema(description = "摘要")
    private String summary;

    @Schema(description = "输入JSON")
    private String inputJson;

    @Schema(description = "报告JSON")
    private String reportJson;

    @Schema(description = "报告Markdown")
    private String reportMarkdown;

    @Schema(description = "异常")
    private String errorMessage;

    @Schema(description = "创建时间")
    private Date createDate;
}
