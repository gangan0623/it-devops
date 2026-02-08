package net.leoch.modules.sys.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.page.BasePage;

/**
 * 参数管理分页请求
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(name = "SysParamsPageRequest")
public class SysParamsPageRequest extends BasePage {

    @Schema(description = "参数编码")
    private String paramCode;
}
