package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.page.BasePage;

/**
 * Linux host page request.
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(name = "LinuxHostPageRequest")
public class LinuxHostPageRequest extends BasePage {

    @Schema(description = "Instance")
    private String instance;

    @Schema(description = "Name")
    private String name;

    @Schema(description = "Area name")
    private String areaName;

    @Schema(description = "Site location")
    private String siteLocation;

    @Schema(description = "Group name")
    private String menuName;

    @Schema(description = "Host type")
    private String type;

    @Schema(description = "Status")
    private String status;
}
