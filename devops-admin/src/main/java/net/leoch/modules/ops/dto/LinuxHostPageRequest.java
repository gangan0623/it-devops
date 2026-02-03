package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * Linux host page request.
 */
@Data
@Schema(name = "LinuxHostPageRequest")
public class LinuxHostPageRequest implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "Page number, starting at 1")
    private String page;

    @Schema(description = "Page size")
    private String limit;

    @Schema(description = "Order field")
    private String orderField;

    @Schema(description = "Order direction: asc, desc")
    private String order;

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

    @Schema(description = "Status")
    private String status;
}
