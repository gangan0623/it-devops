package net.leoch.modules.alert.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.util.Date;

/**
 * 告警模板
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Data
@TableName("tb_alert_template")
public class AlertTemplateEntity {

    private Long id;

    private String name;

    private String emailSubject;

    private String emailHtml;

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
