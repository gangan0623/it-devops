package net.leoch.modules.ops.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 备份差异行
 */
@Data
@Schema(name = "DeviceBackupDiffLineRsp")
public class DeviceBackupDiffLineRsp implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "类型(add/del/same)")
    private String type;

    @Schema(description = "左侧行号")
    private Integer leftLineNo;

    @Schema(description = "右侧行号")
    private Integer rightLineNo;

    @Schema(description = "内容")
    private String content;
}
