package net.leoch.modules.ops.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.util.Date;

/**
 * 备份节点表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Data
@TableName("tb_backup_agent")
public class BackupAgentEntity {

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
     * Token
     */
	private String token;
    /**
     * 状态 0禁用 1启用
     */
	private Integer status;
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
