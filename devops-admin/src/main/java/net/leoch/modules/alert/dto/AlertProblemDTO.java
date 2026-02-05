package net.leoch.modules.alert.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.media.SchemaProperty;
import lombok.Data;

import java.io.Serializable;
import java.util.Date;

/**
 * 问题列表
 */
@Data
@Schema(name = "问题列表")
public class AlertProblemDTO implements Serializable {
    private static final long serialVersionUID = 1L;

    @SchemaProperty(name = "告警记录ID")
    private Long id;

    @SchemaProperty(name = "时间轴")
    private Date startsAt;

    @SchemaProperty(name = "恢复时间")
    private Date endsAt;

    @SchemaProperty(name = "严重性")
    private String severity;

    @SchemaProperty(name = "状态 auto/manual/problem")
    private String status;

    @SchemaProperty(name = "设备类型 linux/windows/business/unknown")
    private String hostType;

    @SchemaProperty(name = "主机名称")
    private String hostName;

    @SchemaProperty(name = "主机实例")
    private String instance;

    @SchemaProperty(name = "问题")
    private String problem;

    @SchemaProperty(name = "告警名称")
    private String alertName;

    @SchemaProperty(name = "摘要")
    private String summary;

    @SchemaProperty(name = "描述")
    private String description;

    @SchemaProperty(name = "持续时间")
    private String duration;

    @SchemaProperty(name = "确定状态")
    private String ackStatus;

    @SchemaProperty(name = "动作信息")
    private String action;

    @SchemaProperty(name = "动作时间")
    private Date actionTime;

    @SchemaProperty(name = "动作媒介")
    private String actionMedia;

    @SchemaProperty(name = "动作接收人")
    private String actionReceivers;

    @SchemaProperty(name = "动作发送状态")
    private String actionSendStatus;

    @SchemaProperty(name = "确定人")
    private String ackOperator;

    @SchemaProperty(name = "确定时间")
    private Date ackTime;

    @SchemaProperty(name = "确定消息")
    private String ackMessage;

    @SchemaProperty(name = "关闭人")
    private String closeOperator;

    @SchemaProperty(name = "关闭时间")
    private Date closeTime;

    @SchemaProperty(name = "关闭消息")
    private String closeMessage;
}
