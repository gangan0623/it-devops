package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 设备备份ID请求
 */
@Data
@Schema(name = "DeviceBackupIdRequest")
public class DeviceBackupIdRequest implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "主键ID")
    private Long id;

    public static DeviceBackupIdRequest of(Long id) {
        DeviceBackupIdRequest req = new DeviceBackupIdRequest();
        req.setId(id);
        return req;
    }
}
