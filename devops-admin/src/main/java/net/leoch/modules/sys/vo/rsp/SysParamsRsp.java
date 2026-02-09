

package net.leoch.modules.sys.vo.rsp;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Null;
import lombok.Data;
import net.leoch.common.data.validator.group.AddGroup;
import net.leoch.common.data.validator.group.DefaultGroup;
import net.leoch.common.data.validator.group.UpdateGroup;

import java.io.Serializable;
import java.util.Date;

/**
 * 参数管理
 *
 * @author Taohongqiang
 * @since 1.0.0
 */
@Data
@Schema(title = "参数管理")
public class SysParamsRsp implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(title = "id")
    @Null(message="{id.null}", groups = AddGroup.class)
    @NotNull(message="{id.require}", groups = UpdateGroup.class)
    private Long id;

    @Schema(title = "参数编码")
    @NotBlank(message="{sysparams.paramcode.require}", groups = DefaultGroup.class)
    private String paramCode;

    @Schema(title = "参数值")
    @NotBlank(message="{sysparams.paramvalue.require}", groups = DefaultGroup.class)
    private String paramValue;

    @Schema(title = "备注")
    private String remark;

    @Schema(title = "创建时间")
    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private Date createDate;

    @Schema(title = "更新时间")
    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private Date updateDate;

}
