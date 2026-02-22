package net.leoch.modules.sys.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
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
 * 参数管理请求
 *
 * @author Taohongqiang
 */
@Data
@Schema(title = "参数管理请求")
public class SysParamsReq implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(title = "id")
    @Null(message = "{id.null}", groups = AddGroup.class)
    @NotNull(message = "{id.require}", groups = UpdateGroup.class)
    private Long id;

    @Schema(title = "参数编码")
    @NotBlank(message = "{sysparams.paramcode.require}", groups = DefaultGroup.class)
    private String paramCode;

    @Schema(title = "参数值")
    @NotBlank(message = "{sysparams.paramvalue.require}", groups = DefaultGroup.class)
    private String paramValue;

    @Schema(title = "备注")
    private String remark;
}
