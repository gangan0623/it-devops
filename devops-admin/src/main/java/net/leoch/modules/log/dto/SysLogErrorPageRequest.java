package net.leoch.modules.log.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.page.BasePage;

/**
 * 异常日志分页请求
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(name = "SysLogErrorPageRequest")
public class SysLogErrorPageRequest extends BasePage {
}
