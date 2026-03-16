package net.leoch.modules.ops.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * 备份对比内容（原始文本，由前端计算 diff）
 */
@Data
@Schema(name = "NetworkDeviceBackupDiffContentRsp")
public class NetworkDeviceBackupDiffContentRsp implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "左侧文件内容（历史版本）")
    private String leftContent;

    @Schema(description = "右侧文件内容（当前/较新版本）")
    private String rightContent;
}
