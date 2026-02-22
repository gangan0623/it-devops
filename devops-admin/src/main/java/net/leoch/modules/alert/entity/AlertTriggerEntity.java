package net.leoch.modules.alert.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.util.Date;

/**
 * 告警触发器
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Data
@TableName("tb_alert_trigger")
public class AlertTriggerEntity {

    /**
     * 主键ID
     */
    private Long id;

    /**
     * 名称
     */
    private String name;

    /**
     * templateID
     */
    private Long templateId;

    /**
     * mediaID
     */
    private Long mediaId;

    /**
     * receiverUsersID
     */
    private String receiverUserIds;

    /**
     * 严重程度
     */
    private String severity;

    /**
     * matchLabels
     */
    private String matchLabels;

    /**
     * 状态（0停用 1启用）
     */
    private Integer status;

    @TableField(fill = FieldFill.INSERT)
    /**
     * 创建者ID
     */
    private Long creator;

    @TableField(fill = FieldFill.INSERT)
    /**
     * 创建时间
     */
    private Date createDate;

    @TableField(fill = FieldFill.INSERT_UPDATE)
    /**
     * 更新者ID
     */
    private Long updater;

    @TableField(fill = FieldFill.INSERT_UPDATE)
    /**
     * 更新时间
     */
    private Date updateDate;
}
