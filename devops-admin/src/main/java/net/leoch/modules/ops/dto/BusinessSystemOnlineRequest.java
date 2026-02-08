package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 业务系统在线状态请求
 */
@Data
@Schema(name = "BusinessSystemOnlineRequest")
public class BusinessSystemOnlineRequest implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "地址")
    private String instance;
}
