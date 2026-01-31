package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * Windows主机状态更新请求
 */
@Data
@Schema(name = "WindowHostStatusUpdateRequest")
public class WindowHostStatusUpdateRequest implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID数组")
    private Long[] ids;

    @Schema(description = "状态")
    private Integer status;
}
