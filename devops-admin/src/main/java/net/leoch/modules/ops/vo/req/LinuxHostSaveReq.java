package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import net.leoch.modules.ops.vo.rsp.LinuxHostRsp;

/**
 * Linux主机保存请求
 */
@Schema(name = "LinuxHostSaveReq")
public class LinuxHostSaveReq extends LinuxHostRsp  {
    @Serial
    private static final long serialVersionUID = 1L;
}
