package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * Windows主机分页请求
 */
@Data
@Schema(name = "WindowHostPageRequest")
public class WindowHostPageRequest implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "当前页码，从1开始")
    private String page;

    @Schema(description = "每页显示记录数")
    private String limit;

    @Schema(description = "排序字段")
    private String orderField;

    @Schema(description = "排序方式，可选值(asc、desc)")
    private String order;

    @Schema(description = "地址")
    private String instance;

    @Schema(description = "名称")
    private String name;

    @Schema(description = "区域名称")
    private String areaName;
}
