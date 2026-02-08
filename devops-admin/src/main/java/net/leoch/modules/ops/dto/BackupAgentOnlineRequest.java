package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 备份节点在线状态请求
 */
@Data
@Schema(name = "BackupAgentOnlineRequest")
public class BackupAgentOnlineRequest implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "地址")
    private String instance;
}
