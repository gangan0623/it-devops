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
 * 监控组件更新请求
 *
 * @author Taohongqiang
 */
@Data
@Schema(name = "MonitorComponentUpdateReq")
public class MonitorComponentUpdateReq implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(title = "id")
    @NotNull(message = "{id.require}", groups = UpdateGroup.class)
    private Long id;

    @Schema(title = "名称")
    @NotBlank(message = "名称不能为空", groups = DefaultGroup.class)
    private String name;

    @Schema(title = "类型")
    private String type;

    @Schema(title = "IP")
    private String ip;

    @Schema(title = "端口")
    private Integer port;

    @Schema(title = "Web地址")
    private String webUrl;
}
