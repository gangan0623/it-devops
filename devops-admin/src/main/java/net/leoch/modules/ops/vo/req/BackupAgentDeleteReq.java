package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 备份节点删除请求
 */
@Data
@Schema(name = "BackupAgentDeleteReq")
public class BackupAgentDeleteReq implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID数组")
    private Long[] ids;

    public static BackupAgentDeleteReq of(Long[] ids) {
        BackupAgentDeleteReq req = new BackupAgentDeleteReq();
        req.setIds(ids);
        return req;
    }
}
