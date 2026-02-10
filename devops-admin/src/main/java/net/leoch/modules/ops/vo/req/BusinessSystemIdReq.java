package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * 业务系统ID请求
 */
@Data
@Schema(name = "BusinessSystemIdReq")
public class BusinessSystemIdReq implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "主键ID")
    private Long id;

    public static BusinessSystemIdReq of(Long id) {
        BusinessSystemIdReq req = new BusinessSystemIdReq();
        req.setId(id);
        return req;
    }
}
