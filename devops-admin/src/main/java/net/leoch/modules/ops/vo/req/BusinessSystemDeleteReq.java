package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 业务系统删除请求
 */
@Data
@Schema(name = "BusinessSystemDeleteReq")
public class BusinessSystemDeleteReq implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID数组")
    private Long[] ids;
}
