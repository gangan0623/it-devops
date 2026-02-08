package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.page.BasePage;

/**
 * 备份节点分页请求
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(name = "BackupAgentPageRequest")
public class BackupAgentPageRequest extends BasePage {

    @Schema(description = "地址")
    private String instance;

    @Schema(description = "名称")
    private String name;

    @Schema(description = "区域名称")
    private String areaName;

    @Schema(description = "状态")
    private String status;
}
