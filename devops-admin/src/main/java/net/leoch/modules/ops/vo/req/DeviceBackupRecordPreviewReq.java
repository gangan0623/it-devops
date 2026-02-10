package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * 设备备份预览请求
 */
@Data
@Schema(name = "DeviceBackupRecordPreviewReq")
public class DeviceBackupRecordPreviewReq implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "备份URL")
    private String url;
}
