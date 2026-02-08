package net.leoch.modules.ops.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.List;
import java.util.Map;

/**
 * Prometheus SD 响应
 */
@Data
@Schema(name = "PrometheusSdRsp")
public class PrometheusSdRsp implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "targets")
    private List<String> targets;

    @Schema(description = "labels")
    private Map<String, Object> labels;
}
