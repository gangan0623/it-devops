package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;
import java.util.Date;

/**
 * 备份统计
 */
@Data
@Schema(name = "DashboardBackupStats")
public class DashboardBackupStats implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "备份轮次")
    private Integer round;

    @Schema(description = "备份总数")
    private Long total;

    @Schema(description = "成功数量")
    private Long success;

    @Schema(description = "失败数量")
    private Long fail;

    @Schema(description = "最近备份时间")
    private Date lastTime;
}
