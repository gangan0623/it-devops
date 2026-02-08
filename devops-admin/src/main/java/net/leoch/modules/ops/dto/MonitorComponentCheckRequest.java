package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 监控组件唯一校验请求
 */
@Data
@Schema(name = "MonitorComponentCheckRequest")
public class MonitorComponentCheckRequest implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "IP")
    private String ip;

    @Schema(description = "端口")
    private Integer port;

    @Schema(description = "名称")
    private String name;

    @Schema(description = "排除ID")
    private Long id;
}
