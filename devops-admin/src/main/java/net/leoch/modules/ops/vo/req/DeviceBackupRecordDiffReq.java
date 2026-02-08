package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 设备备份历史对比请求
 */
@Data
@Schema(name = "DeviceBackupRecordDiffReq")
public class DeviceBackupRecordDiffReq implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "左侧历史ID")
    private Long leftId;

    @Schema(description = "右侧历史ID")
    private Long rightId;
}
