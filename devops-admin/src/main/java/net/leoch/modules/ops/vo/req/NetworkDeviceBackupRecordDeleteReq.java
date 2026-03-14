package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * 设备备份记录删除请求
 */
@Data
@Schema(name = "NetworkDeviceBackupRecordDeleteReq")
public class NetworkDeviceBackupRecordDeleteReq implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID数组")
    private Long[] ids;

    public static NetworkDeviceBackupRecordDeleteReq of(Long[] ids) {
        NetworkDeviceBackupRecordDeleteReq req = new NetworkDeviceBackupRecordDeleteReq();
        req.setIds(ids);
        return req;
    }
}
