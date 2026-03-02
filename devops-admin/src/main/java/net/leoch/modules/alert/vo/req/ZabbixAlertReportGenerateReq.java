package net.leoch.modules.alert.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.Date;

@Data
@Schema(title = "Zabbix告警报告生成请求")
public class ZabbixAlertReportGenerateReq implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "周期类型 day/week")
    private String periodType;

    @Schema(description = "周期开始")
    private Date periodStart;

    @Schema(description = "周期结束")
    private Date periodEnd;
}
