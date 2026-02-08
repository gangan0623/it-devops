package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * Windows主机ID请求
 */
@Data
@Schema(name = "WindowHostIdReq")
public class WindowHostIdReq implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "主键ID")
    private Long id;

    public static WindowHostIdReq of(Long id) {
        WindowHostIdReq req = new WindowHostIdReq();
        req.setId(id);
        return req;
    }
}
