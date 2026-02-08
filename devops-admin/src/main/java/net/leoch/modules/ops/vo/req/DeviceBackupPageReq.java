package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.page.BasePage;

/**
 * 设备备份分页请求
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(name = "DeviceBackupPageReq")
public class DeviceBackupPageReq extends BasePage {

    @Schema(description = "地址")
    private String instance;

    @Schema(description = "名称")
    private String name;

    @Schema(description = "区域名称")
    private String areaName;

    @Schema(description = "分组名称")
    private String groupName;

    @Schema(description = "设备型号")
    private String deviceModel;

    @Schema(description = "状态")
    private String status;

    @Schema(description = "备份节点ID")
    private String agentId;
}
