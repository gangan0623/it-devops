package net.leoch.modules.ops.vo.req;
import net.leoch.modules.ops.vo.rsp.MonitorComponentRsp;

import io.swagger.v3.oas.annotations.media.Schema;

/**
 * 监控组件保存请求
 */
@Schema(name = "MonitorComponentSaveReq")
public class MonitorComponentSaveReq extends MonitorComponentRsp  {
    @Serial
    private static final long serialVersionUID = 1L;
}
