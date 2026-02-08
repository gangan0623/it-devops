package net.leoch.modules.ops.vo.req;
import net.leoch.modules.ops.vo.rsp.DeviceBackupRsp;

import io.swagger.v3.oas.annotations.media.Schema;

/**
 * 设备备份更新请求
 */
@Schema(name = "DeviceBackupUpdateReq")
public class DeviceBackupUpdateReq extends DeviceBackupRsp {
    private static final long serialVersionUID = 1L;
}
