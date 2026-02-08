package net.leoch.modules.ops.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 设备差异项
 */
@Data
@Schema(name = "DashboardDeviceDiffItemRsp")
public class DashboardDeviceDiffItemRsp implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "IP")
    private String ip;

    @Schema(description = "名称")
    private String name;
}
