package net.leoch.modules.alert.dto;

import io.swagger.v3.oas.annotations.media.SchemaProperty;
import lombok.Data;

/**
 * 告警记录操作请求
 */
@Data
public class AlertRecordActionRequest {

    @SchemaProperty(name = "级别 info/warning/critical")
    private String severity;

    @SchemaProperty(name = "抑制天数")
    private Integer days;

    @SchemaProperty(name = "消息")
    private String message;
}
