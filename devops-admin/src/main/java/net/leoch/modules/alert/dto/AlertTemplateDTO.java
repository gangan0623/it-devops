package net.leoch.modules.alert.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.media.SchemaProperty;
import lombok.Data;

import java.io.Serializable;
import java.util.Date;

/**
 * 告警模板
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Data
@Schema(name = "告警模板")
public class AlertTemplateDTO implements Serializable {
    private static final long serialVersionUID = 1L;

    @SchemaProperty(name = "ID")
    private Long id;

	@SchemaProperty(name = "模板名称")
	private String name;

	@SchemaProperty(name = "主题")
	private String emailSubject;

	@SchemaProperty(name = "HTML内容")
	private String emailHtml;

    @SchemaProperty(name = "状态 0禁用 1启用")
    private Integer status;

    @SchemaProperty(name = "创建者")
    private Long creator;

    @SchemaProperty(name = "创建时间")
    private Date createDate;

    @SchemaProperty(name = "更新者")
    private Long updater;

    @SchemaProperty(name = "更新时间")
    private Date updateDate;
}
