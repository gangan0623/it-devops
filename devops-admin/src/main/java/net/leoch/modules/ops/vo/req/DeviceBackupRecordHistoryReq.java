package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 设备备份历史查询请求
 */
@Data
@Schema(name = "DeviceBackupRecordHistoryReq")
public class DeviceBackupRecordHistoryReq implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "设备IP")
    private String ip;

    @Schema(description = "返回条数")
    private Integer limit;
}
