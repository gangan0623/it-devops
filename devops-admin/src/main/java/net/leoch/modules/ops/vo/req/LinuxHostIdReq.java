package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * Linux主机ID请求
 */
@Data
@Schema(name = "LinuxHostIdReq")
public class LinuxHostIdReq implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "主键ID")
    private Long id;

    public static LinuxHostIdReq of(Long id) {
        LinuxHostIdReq req = new LinuxHostIdReq();
        req.setId(id);
        return req;
    }
}
