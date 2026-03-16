package net.leoch.modules.alert.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.util.Date;

/**
 * 告警Webhook原始事件
 */
@Data
@TableName("tb_alert_webhook_event")
public class AlertWebhookEventEntity {
    private Long id;
    private String source;
    private String receiver;
    private String status;
    private String payloadJson;

    @TableField(fill = FieldFill.INSERT)
    private Long creator;
    @TableField(fill = FieldFill.INSERT)
    private Date createDate;
}
