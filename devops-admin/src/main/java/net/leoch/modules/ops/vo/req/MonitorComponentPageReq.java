package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.page.BasePage;

/**
 * 监控组件分页请求
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(name = "MonitorComponentPageReq")
public class MonitorComponentPageReq extends BasePage {

    @Schema(description = "名称")
    private String name;

    @Schema(description = "类型")
    private String type;

    @Schema(description = "IP")
    private String ip;
}
