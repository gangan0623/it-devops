package net.leoch.modules.alert.vo.req;

import io.swagger.v3.oas.annotations.media.SchemaProperty;
import lombok.Data;

/**
 * 告警记录操作请求
 */
@Data
public class AlertRecordActionReq {

    @SchemaProperty(name = "级别 info/warning/critical")
    private String severity;

    @SchemaProperty(name = "抑制天数")
    private Integer days;

    @SchemaProperty(name = "消息")
    private String message;
}
