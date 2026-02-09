package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 设备备份记录删除请求
 */
@Data
@Schema(name = "DeviceBackupRecordDeleteReq")
public class DeviceBackupRecordDeleteReq implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID数组")
    private Long[] ids;

    public static DeviceBackupRecordDeleteReq of(Long[] ids) {
        DeviceBackupRecordDeleteReq req = new DeviceBackupRecordDeleteReq();
        req.setIds(ids);
        return req;
    }
}
