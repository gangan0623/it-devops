package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * 备份节点ID请求
 */
@Data
@Schema(name = "NetworkBackupAgentIdReq")
public class NetworkBackupAgentIdReq implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "主键ID")
    private Long id;

    public static NetworkBackupAgentIdReq of(Long id) {
        NetworkBackupAgentIdReq req = new NetworkBackupAgentIdReq();
        req.setId(id);
        return req;
    }
}
