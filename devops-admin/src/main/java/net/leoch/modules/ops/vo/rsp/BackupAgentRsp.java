package net.leoch.modules.ops.vo.rsp;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.media.SchemaProperty;
import lombok.Data;

import java.io.Serializable;
import java.util.Date;


/**
 * 备份节点表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Data
@Schema(name = "备份节点表")
public class BackupAgentRsp implements Serializable {
    private static final long serialVersionUID = 1L;

	@SchemaProperty(name = "主键ID")
	private Long id;

	@SchemaProperty(name = "地址")
	private String instance;

	@SchemaProperty(name = "名称")
	private String name;

	@SchemaProperty(name = "区域名称")
	private String areaName;

	@SchemaProperty(name = "Token")
	@JsonProperty(access = JsonProperty.Access.WRITE_ONLY)
	private String token;

	@SchemaProperty(name = "状态 0禁用 1启用")
	private Integer status;

	@SchemaProperty(name = "在线状态")
	private Boolean onlineStatus;

	@SchemaProperty(name = "创建者")
	private Long creator;

	@SchemaProperty(name = "创建时间")
	private Date createDate;

	@SchemaProperty(name = "更新者")
	private Long updater;

	@SchemaProperty(name = "更新时间")
	private Date updateDate;


}
