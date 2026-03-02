package net.leoch.modules.alert.entity;

import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.util.Date;

/**
 * Zabbix 告警 AI 报告
 */
@Data
@TableName("tb_zabbix_alert_ai_report")
public class ZabbixAlertAiReportEntity {
    private Long id;
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
