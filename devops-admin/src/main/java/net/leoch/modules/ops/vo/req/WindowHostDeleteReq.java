package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * Windows主机删除请求
 */
@Data
@Schema(name = "WindowHostDeleteReq")
public class WindowHostDeleteReq implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID数组")
    private Long[] ids;
}
