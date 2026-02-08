package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 业务系统删除请求
 */
@Data
@Schema(name = "BusinessSystemDeleteRequest")
public class BusinessSystemDeleteRequest implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID数组")
    private Long[] ids;
}
