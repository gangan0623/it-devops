package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

@Data
@Schema(name = "NetworkHostBackupSaveReq")
public class NetworkHostBackupSaveReq implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "网络设备ID")
    private Long networkHostId;

    @Schema(description = "用户名")
    private String username;

    @Schema(description = "密码")
    private String password;

    @Schema(description = "备份节点ID")
    private Long agentId;

    @Schema(description = "是否启用备份 0否 1是")
    private Integer backupEnabled;
}

