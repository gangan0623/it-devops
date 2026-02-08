package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.page.BasePage;

/**
 * 设备备份记录分页请求
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(name = "DeviceBackupRecordPageRequest")
public class DeviceBackupRecordPageRequest extends BasePage {

    @Schema(description = "主机名")
    private String name;

    @Schema(description = "IP")
    private String ip;

    @Schema(description = "状态")
    private String status;
}
