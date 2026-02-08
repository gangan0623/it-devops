package net.leoch.modules.sys.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.page.BasePage;

/**
 * 文件上传分页请求
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(name = "SysOssPageReq")
public class SysOssPageReq extends BasePage {
}
