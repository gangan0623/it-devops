package net.leoch.modules.alert.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.page.BasePage;

/**
 * 告警触发器分页请求
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(name = "AlertTriggerPageReq")
public class AlertTriggerPageReq extends BasePage {

    @Schema(description = "名称")
    private String name;
}
