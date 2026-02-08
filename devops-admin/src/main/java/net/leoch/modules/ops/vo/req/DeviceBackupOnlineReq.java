package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 设备备份在线状态请求
 */
@Data
@Schema(name = "DeviceBackupOnlineReq")
public class DeviceBackupOnlineReq implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "地址")
    private String instance;
}
