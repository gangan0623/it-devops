package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 设备备份ID请求
 */
@Data
@Schema(name = "DeviceBackupIdReq")
public class DeviceBackupIdReq implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "主键ID")
    private Long id;

    public static DeviceBackupIdReq of(Long id) {
        DeviceBackupIdReq req = new DeviceBackupIdReq();
        req.setId(id);
        return req;
    }
}
