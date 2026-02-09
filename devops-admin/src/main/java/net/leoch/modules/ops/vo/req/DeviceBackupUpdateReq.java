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
 * 设备备份更新请求
 *
 * @author Taohongqiang
 */
@Data
@Schema(name = "DeviceBackupUpdateReq")
public class DeviceBackupUpdateReq implements Serializable {
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

    @Schema(title = "用户名")
    private String username;

    @Schema(title = "密码")
    private String password;

    @Schema(title = "区域名称")
    private String areaName;

    @Schema(title = "分组名称")
    private String groupName;

    @Schema(title = "设备型号")
    private String deviceModel;

    @Schema(title = "状态 0禁用 1启用")
    private Integer status;

    @Schema(title = "节点ID")
    private Long agentId;
}
