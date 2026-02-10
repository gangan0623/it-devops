package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * 监控组件探测请求
 */
@Data
@Schema(name = "MonitorComponentProbeReq")
public class MonitorComponentProbeReq implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "组件ID")
    private Long id;
}
