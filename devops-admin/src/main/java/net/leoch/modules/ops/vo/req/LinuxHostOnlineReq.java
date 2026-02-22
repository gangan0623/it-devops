package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * Linux主机在线状态请求
 */
@Data
@Schema(name = "LinuxHostOnlineReq")
public class LinuxHostOnlineReq implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "地址")
    private String instance;
}
