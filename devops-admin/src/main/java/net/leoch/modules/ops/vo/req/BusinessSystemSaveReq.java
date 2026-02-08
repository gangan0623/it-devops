package net.leoch.modules.ops.vo.req;
import net.leoch.modules.ops.vo.rsp.BusinessSystemRsp;

import io.swagger.v3.oas.annotations.media.Schema;

/**
 * 业务系统保存请求
 */
@Schema(name = "BusinessSystemSaveReq")
public class BusinessSystemSaveReq extends BusinessSystemRsp  {
    @Serial
    private static final long serialVersionUID = 1L;
}
