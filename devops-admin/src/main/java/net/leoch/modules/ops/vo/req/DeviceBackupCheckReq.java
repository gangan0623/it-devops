package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 设备备份唯一校验请求
 */
@Data
@Schema(name = "DeviceBackupCheckReq")
public class DeviceBackupCheckReq implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "地址")
    private String instance;

    @Schema(description = "名称")
    private String name;

    @Schema(description = "排除ID")
    private Long id;
}
