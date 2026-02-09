package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.data.page.BasePage;

/**
 * 设备备份记录分页请求
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(name = "DeviceBackupRecordPageReq")
public class DeviceBackupRecordPageReq extends BasePage {

    @Schema(description = "主机名")
    private String name;

    @Schema(description = "IP")
    private String ip;

    @Schema(description = "状态")
    private String status;
}
