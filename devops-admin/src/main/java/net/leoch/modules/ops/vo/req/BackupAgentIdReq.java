package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * 备份节点ID请求
 */
@Data
@Schema(name = "BackupAgentIdReq")
public class BackupAgentIdReq implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "主键ID")
    private Long id;

    public static BackupAgentIdReq of(Long id) {
        BackupAgentIdReq req = new BackupAgentIdReq();
        req.setId(id);
        return req;
    }
}
