package net.leoch.modules.ops.vo.req;
import net.leoch.modules.ops.vo.rsp.BackupAgentRsp;

import io.swagger.v3.oas.annotations.media.Schema;

/**
 * 备份节点更新请求
 */
@Schema(name = "BackupAgentUpdateReq")
public class BackupAgentUpdateReq extends BackupAgentRsp {
    private static final long serialVersionUID = 1L;
}
