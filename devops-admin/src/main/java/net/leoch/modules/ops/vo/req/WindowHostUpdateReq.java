package net.leoch.modules.ops.vo.req;
import net.leoch.modules.ops.vo.rsp.WindowHostRsp;

import io.swagger.v3.oas.annotations.media.Schema;

/**
 * Windows主机更新请求
 */
@Schema(name = "WindowHostUpdateReq")
public class WindowHostUpdateReq extends WindowHostRsp  {
    @Serial
    private static final long serialVersionUID = 1L;
}
