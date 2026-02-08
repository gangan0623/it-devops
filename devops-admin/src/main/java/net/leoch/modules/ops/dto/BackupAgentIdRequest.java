package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 备份节点ID请求
 */
@Data
@Schema(name = "BackupAgentIdRequest")
public class BackupAgentIdRequest implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "主键ID")
    private Long id;

    public static BackupAgentIdRequest of(Long id) {
        BackupAgentIdRequest req = new BackupAgentIdRequest();
        req.setId(id);
        return req;
    }
}
