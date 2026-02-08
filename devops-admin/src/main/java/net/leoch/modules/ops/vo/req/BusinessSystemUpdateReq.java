package net.leoch.modules.ops.vo.req;
import net.leoch.modules.ops.vo.rsp.BusinessSystemRsp;

import io.swagger.v3.oas.annotations.media.Schema;

/**
 * 业务系统更新请求
 */
@Schema(name = "BusinessSystemUpdateReq")
public class BusinessSystemUpdateReq extends BusinessSystemRsp  {
    @Serial
    private static final long serialVersionUID = 1L;
}
