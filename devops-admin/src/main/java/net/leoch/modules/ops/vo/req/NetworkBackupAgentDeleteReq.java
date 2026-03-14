package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * 备份节点删除请求
 */
@Data
@Schema(name = "NetworkBackupAgentDeleteReq")
public class NetworkBackupAgentDeleteReq implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID数组")
    private Long[] ids;

    public static NetworkBackupAgentDeleteReq of(Long[] ids) {
        NetworkBackupAgentDeleteReq req = new NetworkBackupAgentDeleteReq();
        req.setIds(ids);
        return req;
    }
}
