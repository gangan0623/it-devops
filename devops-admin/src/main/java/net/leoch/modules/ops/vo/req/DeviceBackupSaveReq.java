package net.leoch.modules.ops.vo.req;
import net.leoch.modules.ops.vo.rsp.DeviceBackupRsp;

import io.swagger.v3.oas.annotations.media.Schema;

/**
 * 设备备份保存请求
 */
@Schema(name = "DeviceBackupSaveReq")
public class DeviceBackupSaveReq extends DeviceBackupRsp  {
    @Serial
    private static final long serialVersionUID = 1L;
}
