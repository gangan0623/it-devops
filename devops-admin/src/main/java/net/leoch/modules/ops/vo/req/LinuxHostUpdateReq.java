package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;
import net.leoch.common.validator.group.DefaultGroup;
import net.leoch.common.validator.group.UpdateGroup;

import java.io.Serial;
import java.io.Serializable;

/**
 * Linux主机更新请求
 *
 * @author Taohongqiang
 */
@Data
@Schema(name = "LinuxHostUpdateReq")
public class LinuxHostUpdateReq implements Serializable {
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

    @Schema(title = "站点位置")
    private String siteLocation;

    @Schema(title = "分组名称")
    private String menuName;

    @Schema(title = "子组名称")
    private String subMenuName;

    @Schema(title = "主机类型")
    private String type;

    @Schema(title = "状态 0禁用 1启用")
    private Integer status;
}
