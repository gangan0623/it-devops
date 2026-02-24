package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.data.page.BasePage;

/**
 * 备份节点分页请求
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(name = "BackupAgentPageReq")
public class BackupAgentPageReq extends BasePage {

    @Schema(description = "地址")
    private String instance;

    @Schema(description = "名称")
    private String name;

    @Schema(description = "区域名称")
    private String areaName;

    @Schema(description = "状态")
    private String status;

    @Schema(description = "在线状态")
    private String onlineStatus;
}
