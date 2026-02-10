package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.web.multipart.MultipartFile;

import java.io.Serial;
import java.io.Serializable;

/**
 * 设备备份导入请求
 */
@Data
@Schema(name = "DeviceBackupImportReq")
public class DeviceBackupImportReq implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "导入文件")
    private MultipartFile file;
}
