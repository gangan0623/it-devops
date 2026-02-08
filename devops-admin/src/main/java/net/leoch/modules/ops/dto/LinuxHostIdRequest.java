package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * Linux主机ID请求
 */
@Data
@Schema(name = "LinuxHostIdRequest")
public class LinuxHostIdRequest implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "主键ID")
    private Long id;

    public static LinuxHostIdRequest of(Long id) {
        LinuxHostIdRequest req = new LinuxHostIdRequest();
        req.setId(id);
        return req;
    }
}
