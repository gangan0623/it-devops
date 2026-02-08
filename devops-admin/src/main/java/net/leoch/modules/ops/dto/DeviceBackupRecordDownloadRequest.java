package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 设备备份下载请求
 */
@Data
@Schema(name = "DeviceBackupRecordDownloadRequest")
public class DeviceBackupRecordDownloadRequest implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "备份URL")
    private String url;
}
