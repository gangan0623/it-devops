package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.web.multipart.MultipartFile;

import java.io.Serializable;

/**
 * Windows主机导入请求
 */
@Data
@Schema(name = "WindowHostImportReq")
public class WindowHostImportReq implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "导入文件")
    private MultipartFile file;
}
