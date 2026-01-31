package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 设备备份历史与当前对比请求
 */
@Data
@Schema(name = "DeviceBackupRecordDiffCurrentRequest")
public class DeviceBackupRecordDiffCurrentRequest implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "设备IP")
    private String ip;

    @Schema(description = "历史ID")
    private Long historyId;
}
