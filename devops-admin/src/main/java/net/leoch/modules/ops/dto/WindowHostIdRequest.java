package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * Windows主机ID请求
 */
@Data
@Schema(name = "WindowHostIdRequest")
public class WindowHostIdRequest implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "主键ID")
    private Long id;

    public static WindowHostIdRequest of(Long id) {
        WindowHostIdRequest req = new WindowHostIdRequest();
        req.setId(id);
        return req;
    }
}
