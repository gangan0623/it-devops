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

    private Long id;

    private String name;

    private Long templateId;

    private Long mediaId;

    private String receiverUserIds;

    private String severity;

    private String matchLabels;

    private Integer status;

    @TableField(fill = FieldFill.INSERT)
    private Long creator;

    @TableField(fill = FieldFill.INSERT)
    private Date createDate;

    @TableField(fill = FieldFill.INSERT_UPDATE)
    private Long updater;

    @TableField(fill = FieldFill.INSERT_UPDATE)
    private Date updateDate;
}
