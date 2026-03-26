package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * 域名记录ID请求
 */
@Data
@Schema(name = "DomainRecordIdReq")
public class DomainRecordIdReq implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "主键ID")
    @NotNull(message = "ID不能为空")
    private Long id;

    public static DomainRecordIdReq of(Long id) {
        DomainRecordIdReq req = new DomainRecordIdReq();
        req.setId(id);
        return req;
    }
}
