package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 备份节点状态更新请求
 */
@Data
@Schema(name = "BackupAgentStatusUpdateReq")
public class BackupAgentStatusUpdateReq implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID数组")
    private Long[] ids;

    @Schema(description = "状态")
    private Integer status;
}
