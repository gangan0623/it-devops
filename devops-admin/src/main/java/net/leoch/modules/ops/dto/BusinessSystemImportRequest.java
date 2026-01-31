package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.web.multipart.MultipartFile;

import java.io.Serializable;

/**
 * 业务系统导入请求
 */
@Data
@Schema(name = "BusinessSystemImportRequest")
public class BusinessSystemImportRequest implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "导入文件")
    private MultipartFile file;
}
