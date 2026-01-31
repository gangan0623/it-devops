package net.leoch.modules.alert.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.media.SchemaProperty;
import lombok.Data;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * 告警触发器
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Data
@Schema(name = "告警触发器")
public class AlertTriggerDTO implements Serializable {
    private static final long serialVersionUID = 1L;

    @SchemaProperty(name = "ID")
    private Long id;

    @SchemaProperty(name = "触发器名称")
    private String name;

    @SchemaProperty(name = "模板ID")
    private Long templateId;

    @SchemaProperty(name = "媒介ID")
    private Long mediaId;

    @SchemaProperty(name = "接收用户ID列表")
    private String receiverUserIds;

    @SchemaProperty(name = "接收用户ID列表")
    private List<Long> receiverUserIdList;

    @SchemaProperty(name = "告警级别(逗号分隔)")
    private String severity;

	@SchemaProperty(name = "匹配标签(JSON)")
	private String matchLabels;

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
