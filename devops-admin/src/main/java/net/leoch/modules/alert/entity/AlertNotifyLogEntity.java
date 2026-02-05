package net.leoch.modules.alert.entity;

import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.util.Date;

/**
 * 告警发送日志
 */
@Data
@TableName("tb_alert_notify_log")
public class AlertNotifyLogEntity {

    private Long id;

    private Long recordId;

    private String alertName;

    private String instance;

    private String severity;

    private String mediaName;

    private String receivers;

    private Integer sendStatus;

    private String errorMessage;

    private Date sendTime;
}
