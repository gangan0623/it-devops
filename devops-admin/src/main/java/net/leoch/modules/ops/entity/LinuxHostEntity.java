package net.leoch.modules.ops.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.util.Date;

/**
 * Linux主机表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Data
@TableName("tb_linux_host")
public class LinuxHostEntity {

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
     * 区域名称
     */
	private String areaName;
    /**
     * 站点位置
     */
    private String siteLocation;
    /**
     * 分组名称
     */
	private String menuName;
    /**
     * 子组名称
     */
	private String subMenuName;
    /**
     * 主机类型
     */
    private String type;
    /**
     * 状态 0禁用 1启用
     */
	private Integer status;
    /**
     * 在线状态 0离线 1在线
     */
    private Boolean onlineStatus;
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
