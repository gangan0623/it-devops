package net.leoch.modules.alert.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.media.SchemaProperty;
import lombok.Data;

import java.io.Serializable;
import java.util.Date;

/**
 * 告警记录操作历史
 */
@Data
@Schema(name = "告警记录操作历史")
public class AlertRecordActionRsp implements Serializable {
    private static final long serialVersionUID = 1L;

    @SchemaProperty(name = "ID")
    private Long id;

    @SchemaProperty(name = "告警记录ID")
    private Long recordId;

    @SchemaProperty(name = "动作")
    private String action;

    @SchemaProperty(name = "消息")
    private String message;

    @SchemaProperty(name = "详情")
    private String details;

    @SchemaProperty(name = "操作用户ID")
    private Long creator;

    @SchemaProperty(name = "操作用户")
    private String operatorName;

    @SchemaProperty(name = "操作时间")
    private Date createDate;
}
