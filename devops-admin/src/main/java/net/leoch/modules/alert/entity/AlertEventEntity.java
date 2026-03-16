package net.leoch.modules.alert.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.util.Date;

/**
 * 告警事件流水
 */
@Data
@TableName("tb_alert_event")
public class AlertEventEntity {
    private Long id;
    private Long webhookEventId;
    private String fingerprint;
    private String alertName;
    private String instance;
    private String status;
    private String severity;
    private String summary;
    private String description;
    private Date startsAt;
    private Date endsAt;
    private String receiver;
    private String alertGroup;
    private Date eventTime;
    private Long recordId;

    @TableField(fill = FieldFill.INSERT)
    private Long creator;
    @TableField(fill = FieldFill.INSERT)
    private Date createDate;
}
