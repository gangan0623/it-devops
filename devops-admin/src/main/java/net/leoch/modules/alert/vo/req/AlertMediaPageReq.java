package net.leoch.modules.alert.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.data.page.BasePage;

/**
 * 告警媒介分页请求
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(name = "AlertMediaPageReq")
public class AlertMediaPageReq extends BasePage {

    @Schema(description = "名称")
    private String name;

    @Schema(description = "状态")
    private String status;
}
