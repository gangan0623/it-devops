package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 设备备份记录ID请求
 */
@Data
@Schema(name = "DeviceBackupRecordIdRequest")
public class DeviceBackupRecordIdRequest implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "主键ID")
    private Long id;

    public static DeviceBackupRecordIdRequest of(Long id) {
        DeviceBackupRecordIdRequest req = new DeviceBackupRecordIdRequest();
        req.setId(id);
        return req;
    }
}
