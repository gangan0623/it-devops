package net.leoch.modules.sys.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.data.page.BasePage;

/**
 * 字典类型分页请求
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(name = "SysDictTypePageReq")
public class SysDictTypePageReq extends BasePage {

    @Schema(description = "字典类型")
    private String dictType;

    @Schema(description = "字典名称")
    private String dictName;
}
