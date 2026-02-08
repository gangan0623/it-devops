package net.leoch.modules.alert.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.page.BasePage;

/**
 * 告警触发器分页请求
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(name = "AlertTriggerPageRequest")
public class AlertTriggerPageRequest extends BasePage {

    @Schema(description = "名称")
    private String name;
}
