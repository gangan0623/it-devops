package net.leoch.modules.sys.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Null;
import lombok.Data;
import net.leoch.common.data.validator.group.AddGroup;
import net.leoch.common.data.validator.group.DefaultGroup;
import net.leoch.common.data.validator.group.UpdateGroup;

import java.io.Serial;
import java.io.Serializable;

/**
 * 字典数据请求
 *
 * @author Taohongqiang
 */
@Data
@Schema(title = "字典数据请求")
public class SysDictDataReq implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(title = "id")
    @Null(message = "{id.null}", groups = AddGroup.class)
    @NotNull(message = "{id.require}", groups = UpdateGroup.class)
    private Long id;

    @Schema(title = "字典类型ID")
    @NotNull(message = "{sysdict.type.require}", groups = DefaultGroup.class)
    private Long dictTypeId;

    @Schema(title = "字典标签")
    @NotBlank(message = "{sysdict.label.require}", groups = DefaultGroup.class)
    private String dictLabel;

    @Schema(title = "字典值")
    private String dictValue;

    @Schema(title = "备注")
    private String remark;

    @Schema(title = "排序")
    @Min(value = 0, message = "{sort.number}", groups = DefaultGroup.class)
    private Integer sort;
}
