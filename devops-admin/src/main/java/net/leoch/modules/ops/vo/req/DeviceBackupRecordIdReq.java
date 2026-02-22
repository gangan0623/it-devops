package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * 设备备份记录ID请求
 */
@Data
@Schema(name = "DeviceBackupRecordIdReq")
public class DeviceBackupRecordIdReq implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "主键ID")
    private Long id;

    public static DeviceBackupRecordIdReq of(Long id) {
        DeviceBackupRecordIdReq req = new DeviceBackupRecordIdReq();
        req.setId(id);
        return req;
    }
}
