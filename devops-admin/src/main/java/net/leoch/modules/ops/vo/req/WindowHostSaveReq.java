package net.leoch.modules.ops.vo.req;
import net.leoch.modules.ops.vo.rsp.WindowHostRsp;

import io.swagger.v3.oas.annotations.media.Schema;

/**
 * Windows主机保存请求
 */
@Schema(name = "WindowHostSaveReq")
public class WindowHostSaveReq extends WindowHostRsp  {
    @Serial
    private static final long serialVersionUID = 1L;
}
