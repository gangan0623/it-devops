package net.leoch.modules.alert.entity;

import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.util.Date;

/**
 * Zabbix webhook 接收日志
 */
@Data
@TableName("tb_zabbix_webhook_log")
public class ZabbixWebhookLogEntity {
    private Long id;
    private String source;
    private String requestId;
    private String requestMethod;
    private String requestUri;
    private String queryString;
    private String contentType;
    private String remoteIp;
    private String userAgent;
    private String headersJson;
    private String payloadRaw;
    private Integer payloadSize;
    private String eventId;
    private String triggerId;
    private String host;
    private String severity;
    private String status;
    private Integer processStatus;
    private String processMessage;
    private Date receivedAt;
    private Date createDate;
}

