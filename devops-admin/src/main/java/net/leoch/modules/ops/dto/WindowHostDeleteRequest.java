package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * Windows主机删除请求
 */
@Data
@Schema(name = "WindowHostDeleteRequest")
public class WindowHostDeleteRequest implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID数组")
    private Long[] ids;
}
