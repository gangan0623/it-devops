package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 主机数量统计
 */
@Data
@Schema(name = "DashboardHostCounts")
public class DashboardHostCounts implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "Windows主机数量")
    private Long windows;

    @Schema(description = "Linux主机数量")
    private Long linux;

    @Schema(description = "业务系统数量")
    private Long business;
}
