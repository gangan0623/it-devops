package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 监控组件版本检测请求
 */
@Data
@Schema(name = "MonitorComponentVersionRequest")
public class MonitorComponentVersionRequest implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "组件ID")
    private Long id;
}
