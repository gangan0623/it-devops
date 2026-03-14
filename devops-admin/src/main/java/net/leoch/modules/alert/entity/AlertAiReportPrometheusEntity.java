package net.leoch.modules.alert.entity;

import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.util.Date;

/**
 * Prometheus 告警 AI 报告
 */
@Data
@TableName("tb_alert_ai_report_prometheus")
public class AlertAiReportPrometheusEntity {
    private Long id;
    /** 报告类型：server / http */
    private String reportType;
    private String periodType;
    private Date periodStart;
    private Date periodEnd;
    private String modelName;
    private Integer reportStatus;
    private String summary;
    private String inputJson;
    private String reportJson;
    private String reportMarkdown;
    private String errorMessage;
    private Date createDate;
}
