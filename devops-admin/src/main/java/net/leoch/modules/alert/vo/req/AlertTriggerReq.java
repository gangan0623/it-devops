package net.leoch.modules.alert.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.List;

/**
 * 告警触发器请求
 *
 * @author Taohongqiang
 */
@Data
@Schema(name = "告警触发器请求")
public class AlertTriggerReq implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "触发器名称")
    private String name;

    @Schema(description = "模板ID")
    private Long templateId;

    @Schema(description = "媒介ID")
    private Long mediaId;

    @Schema(description = "接收用户ID列表(逗号分隔)")
    private String receiverUserIds;

    @Schema(description = "接收用户ID列表")
    private List<Long> receiverUserIdList;

    @Schema(description = "告警级别(逗号分隔)")
    private String severity;

    @Schema(description = "匹配标签(JSON)")
    private String matchLabels;

    @Schema(description = "状态 0禁用 1启用")
    private Integer status;
}
