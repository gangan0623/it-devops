package net.leoch.modules.alert.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.media.SchemaProperty;
import lombok.Data;

import java.io.Serializable;

/**
 * 告警媒介测试
 */
@Data
@Schema(name = "告警媒介测试")
public class AlertMediaTestDTO implements Serializable {
    private static final long serialVersionUID = 1L;

    @SchemaProperty(name = "媒介ID")
    private Long mediaId;

    @SchemaProperty(name = "收件人")
    private String to;

    @SchemaProperty(name = "主题")
    private String subject;

    @SchemaProperty(name = "内容")
    private String content;

    @SchemaProperty(name = "HTML内容")
    private String html;
}
