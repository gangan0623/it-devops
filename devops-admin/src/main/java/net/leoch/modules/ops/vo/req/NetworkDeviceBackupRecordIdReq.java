package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * 设备备份记录ID请求
 */
@Data
@Schema(name = "NetworkDeviceBackupRecordIdReq")
public class NetworkDeviceBackupRecordIdReq implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "主键ID")
    private Long id;

    public static NetworkDeviceBackupRecordIdReq of(Long id) {
        NetworkDeviceBackupRecordIdReq req = new NetworkDeviceBackupRecordIdReq();
        req.setId(id);
        return req;
    }
}
