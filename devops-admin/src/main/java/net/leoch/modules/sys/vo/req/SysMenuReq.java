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
import org.hibernate.validator.constraints.Range;

import java.io.Serial;
import java.io.Serializable;

/**
 * 菜单管理请求
 *
 * @author Taohongqiang
 */
@Data
@Schema(title = "菜单管理请求")
public class SysMenuReq implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(title = "id")
    @Null(message = "{id.null}", groups = AddGroup.class)
    @NotNull(message = "{id.require}", groups = UpdateGroup.class)
    private Long id;

    @Schema(title = "上级ID")
    @NotNull(message = "{sysmenu.pid.require}", groups = DefaultGroup.class)
    private Long pid;

    @Schema(title = "菜单名称")
    @NotBlank(message = "sysmenu.name.require", groups = DefaultGroup.class)
    private String name;

    @Schema(title = "菜单URL")
    private String url;

    @Schema(title = "类型  0：菜单   1：按钮")
    @Range(min = 0, max = 1, message = "{sysmenu.type.range}", groups = DefaultGroup.class)
    private Integer menuType;

    @Schema(title = "菜单图标")
    private String icon;

    @Schema(title = "授权(多个用逗号分隔，如：sys:user:list,sys:user:save)")
    private String permissions;

    @Schema(title = "排序")
    @Min(value = 0, message = "{sort.number}", groups = DefaultGroup.class)
    private Integer sort;
}
