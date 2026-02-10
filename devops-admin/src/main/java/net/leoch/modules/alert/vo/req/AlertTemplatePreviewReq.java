package net.leoch.modules.alert.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.media.SchemaProperty;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * 模板预览
 */
@Data
@Schema(name = "模板预览")
public class AlertTemplatePreviewReq implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @SchemaProperty(name = "模板ID")
    private Long templateId;

    @SchemaProperty(name = "原始JSON")
    private String rawJson;
}
