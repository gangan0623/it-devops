package net.leoch.modules.alert.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * 告警模板请求
 *
 * @author Taohongqiang
 */
@Data
@Schema(name = "告警模板请求")
public class AlertTemplateReq implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "模板名称")
    private String name;

    @Schema(description = "主题")
    private String emailSubject;

    @Schema(description = "HTML内容")
    private String emailHtml;

    @Schema(description = "状态 0禁用 1启用")
    private Integer status;
}
