package net.leoch.modules.alert.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.media.SchemaProperty;
import lombok.Data;

import java.io.Serializable;

/**
 * 模板发送测试
 */
@Data
@Schema(name = "模板发送测试")
public class AlertTemplateSendTestDTO implements Serializable {
    private static final long serialVersionUID = 1L;

    @SchemaProperty(name = "模板ID")
    private Long templateId;

    @SchemaProperty(name = "触发器ID")
    private Long triggerId;

    @SchemaProperty(name = "原始JSON")
    private String rawJson;
}
