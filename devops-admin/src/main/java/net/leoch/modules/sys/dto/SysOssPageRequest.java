package net.leoch.modules.sys.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.page.BasePage;

/**
 * 文件上传分页请求
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(name = "SysOssPageRequest")
public class SysOssPageRequest extends BasePage {
}
