package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 业务系统ID请求
 */
@Data
@Schema(name = "BusinessSystemIdRequest")
public class BusinessSystemIdRequest implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "主键ID")
    private Long id;

    public static BusinessSystemIdRequest of(Long id) {
        BusinessSystemIdRequest req = new BusinessSystemIdRequest();
        req.setId(id);
        return req;
    }
}
