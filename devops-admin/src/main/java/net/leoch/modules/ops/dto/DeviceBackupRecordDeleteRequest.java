package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 设备备份记录删除请求
 */
@Data
@Schema(name = "DeviceBackupRecordDeleteRequest")
public class DeviceBackupRecordDeleteRequest implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID数组")
    private Long[] ids;
}
