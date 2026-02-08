package net.leoch.modules.alert.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.media.SchemaProperty;
import lombok.Data;

import java.io.Serializable;
import java.util.Date;

/**
 * 告警记录
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Data
@Schema(name = "告警记录")
public class AlertRecordRsp implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @SchemaProperty(name = "ID")
    private Long id;

    @SchemaProperty(name = "告警名称")
    private String alertName;

    @SchemaProperty(name = "告警状态")
    private String status;

    @SchemaProperty(name = "级别")
    private String severity;

    @SchemaProperty(name = "实例")
    private String instance;

    @SchemaProperty(name = "主机名")
    private String hostName;

    @SchemaProperty(name = "摘要")
    private String summary;

    @SchemaProperty(name = "描述")
    private String description;

    @SchemaProperty(name = "开始时间")
    private Date startsAt;

    @SchemaProperty(name = "结束时间")
    private Date endsAt;

    @SchemaProperty(name = "接收器")
    private String receiver;

    @SchemaProperty(name = "原始JSON")
    private String rawJson;

    @SchemaProperty(name = "是否关闭 0否 1是")
    private Integer closed;

    @SchemaProperty(name = "抑制截止时间")
    private Date suppressedUntil;

    @SchemaProperty(name = "创建者")
    private Long creator;

    @SchemaProperty(name = "创建时间")
    private Date createDate;

    @SchemaProperty(name = "更新者")
    private Long updater;

    @SchemaProperty(name = "更新时间")
    private Date updateDate;
}
