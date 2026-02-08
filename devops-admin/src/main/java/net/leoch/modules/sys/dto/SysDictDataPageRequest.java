package net.leoch.modules.sys.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.page.BasePage;

/**
 * 字典数据分页请求
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(name = "SysDictDataPageRequest")
public class SysDictDataPageRequest extends BasePage {

    @Schema(description = "字典类型ID")
    private String dictTypeId;

    @Schema(description = "字典标签")
    private String dictLabel;

    @Schema(description = "字典值")
    private String dictValue;
}
