package net.leoch.modules.alert.entity;

import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.util.Date;

/**
 * Zabbix 告警事件流水
 */
@Data
@TableName("tb_zabbix_alert_event_history")
public class ZabbixAlertEventHistoryEntity {
    private Long id;
    private String eventId;
    private String triggerId;
    private String hostId;
    private String hostHost;
    private String hostname;
    private String hostIp;
    private String hostGroup;
    private String severityCode;
    private String severityName;
    private String triggerName;
    private String triggerKey;
    private String triggerValue;
    private String itemId;
    private String itemName;
    private String itemValue;
    private String status;
    private Date eventTime;
    private String eventDuration;
    private Long durationSec;
    private Long webhookLogId;
    private String payloadJson;
    private Date createDate;
}
