package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * Windows主机唯一校验请求
 */
@Data
@Schema(name = "WindowHostCheckReq")
public class WindowHostCheckReq implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "地址")
    private String instance;

    @Schema(description = "名称")
    private String name;

    @Schema(description = "ID")
    private Long id;
}
