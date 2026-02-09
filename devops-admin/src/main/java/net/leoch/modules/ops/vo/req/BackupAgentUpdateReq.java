package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;
import net.leoch.common.data.validator.group.DefaultGroup;
import net.leoch.common.data.validator.group.UpdateGroup;

import java.io.Serial;
import java.io.Serializable;

/**
 * 备份节点更新请求
 *
 * @author Taohongqiang
 */
@Data
@Schema(name = "BackupAgentUpdateReq")
public class BackupAgentUpdateReq implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(title = "id")
    @NotNull(message = "{id.require}", groups = UpdateGroup.class)
    private Long id;

    @Schema(title = "地址")
    @NotBlank(message = "地址不能为空", groups = DefaultGroup.class)
    private String instance;

    @Schema(title = "名称")
    @NotBlank(message = "名称不能为空", groups = DefaultGroup.class)
    private String name;

    @Schema(title = "区域名称")
    private String areaName;

    @Schema(title = "Token")
    private String token;

    @Schema(title = "状态 0禁用 1启用")
    private Integer status;
}
