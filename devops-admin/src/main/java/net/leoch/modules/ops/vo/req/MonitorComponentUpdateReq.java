package net.leoch.modules.ops.vo.req;
import net.leoch.modules.ops.vo.rsp.MonitorComponentRsp;

import io.swagger.v3.oas.annotations.media.Schema;

/**
 * 监控组件更新请求
 */
@Schema(name = "MonitorComponentUpdateReq")
public class MonitorComponentUpdateReq extends MonitorComponentRsp {
    private static final long serialVersionUID = 1L;
}
