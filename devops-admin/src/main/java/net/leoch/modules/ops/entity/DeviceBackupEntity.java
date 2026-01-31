package net.leoch.modules.ops.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.util.Date;

/**
 * 设备备份表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Data
@TableName("tb_device_backup")
public class DeviceBackupEntity {

    /**
     * 主键ID
     */
	private Long id;
    /**
     * 地址
     */
	private String instance;
    /**
     * 名称
     */
	private String name;
    /**
     * 用户名
     */
	private String username;
    /**
     * 密码
     */
	private String password;
    /**
     * 区域名称
     */
	private String areaName;
    /**
     * 分组名称
     */
	private String groupName;
    /**
     * 设备型号
     */
	private String deviceModel;
    /**
     * 状态 0禁用 1启用
     */
	private Integer status;
    /**
     * 节点ID
     */
	private Long agentId;
    /**
     * 创建者
     */
    @TableField(fill = FieldFill.INSERT)
	private Long creator;
    /**
     * 创建时间
     */
    @TableField(fill = FieldFill.INSERT)
	private Date createDate;
    /**
     * 更新者
     */
    @TableField(fill = FieldFill.INSERT_UPDATE)
	private Long updater;
    /**
     * 更新时间
     */
    @TableField(fill = FieldFill.INSERT_UPDATE)
	private Date updateDate;
}
