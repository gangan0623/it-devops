package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 设备备份下载请求
 */
@Data
@Schema(name = "DeviceBackupRecordDownloadReq")
public class DeviceBackupRecordDownloadReq implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "备份URL")
    private String url;
}
