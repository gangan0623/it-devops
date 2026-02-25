package net.leoch.modules.ops.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

@Data
@Schema(title = "删除联动统计响应")
public class OpsDeleteCascadeRsp implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(title = "删除设备数量")
    private Integer deletedDevices = 0;

    @Schema(title = "删除告警记录数量")
    private Integer deletedAlertRecords = 0;

    @Schema(title = "删除告警动作数量")
    private Integer deletedAlertActions = 0;

    @Schema(title = "删除备份记录数量")
    private Integer deletedBackupRecords = 0;

    @Schema(title = "删除备份历史数量")
    private Integer deletedBackupHistories = 0;

    @Schema(title = "删除MinIO文本文件数量")
    private Integer deletedMinioTxtFiles = 0;
}
