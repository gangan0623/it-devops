package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import net.leoch.modules.ops.vo.rsp.LinuxHostRsp;

/**
 * Linux主机更新请求
 */
@Schema(name = "LinuxHostUpdateReq")
public class LinuxHostUpdateReq extends LinuxHostRsp {
    private static final long serialVersionUID = 1L;
}
