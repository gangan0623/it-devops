package net.leoch.modules.alert.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.page.BasePage;

/**
 * 告警记录分页请求
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(name = "AlertRecordPageRequest")
public class AlertRecordPageRequest extends BasePage {

    @Schema(description = "告警名称")
    private String alertName;

    @Schema(description = "实例")
    private String instance;

    @Schema(description = "主机名")
    private String hostName;

    @Schema(description = "严重性")
    private String severity;

    @Schema(description = "状态")
    private String status;

    @Schema(description = "设备类型")
    private String deviceType;
}
