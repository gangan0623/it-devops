package net.leoch.modules.ops.vo.rsp;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.media.SchemaProperty;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.Date;


/**
 * 设备备份表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Data
@Schema(name = "设备备份表")
public class DeviceBackupRsp implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

	@SchemaProperty(name = "主键ID")
	private Long id;

	@SchemaProperty(name = "地址")
	private String instance;

	@SchemaProperty(name = "名称")
	private String name;

	@SchemaProperty(name = "用户名")
	private String username;

	@SchemaProperty(name = "密码")
	@JsonProperty(access = JsonProperty.Access.WRITE_ONLY)
	private String password;

	@SchemaProperty(name = "区域名称")
	private String areaName;

	@SchemaProperty(name = "分组名称")
	private String groupName;

	@SchemaProperty(name = "设备型号")
	private String deviceModel;

	@SchemaProperty(name = "状态 0禁用 1启用")
	private Integer status;

	@SchemaProperty(name = "节点ID")
	private Long agentId;

	@SchemaProperty(name = "节点名称")
	private String agentName;

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
