package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * Prometheus SD 请求
 */
@Data
@Schema(name = "PrometheusSdReq")
public class PrometheusSdReq implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "区域代码")
    private String area;

    public static PrometheusSdReq ofArea(String area) {
        PrometheusSdReq req = new PrometheusSdReq();
        req.setArea(area);
        return req;
    }
}
