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

    /**
     * 主键ID
     */
    private Long id;

    /**
     * recordID
     */
    private Long recordId;

    /**
     * 名称
     */
    private String alertName;

    /**
     * instance
     */
    private String instance;

    /**
     * 严重程度
     */
    private String severity;

    /**
     * 名称
     */
    private String mediaName;

    /**
     * receivers
     */
    private String receivers;

    /**
     * 状态
     */
    private Integer sendStatus;

    /**
     * errorMessage
     */
    private String errorMessage;

    /**
     * sendTime时间
     */
    private Date sendTime;
}
