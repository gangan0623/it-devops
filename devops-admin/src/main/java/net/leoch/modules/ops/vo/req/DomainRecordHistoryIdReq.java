package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * 域名记录操作历史ID请求
 */
@Data
@Schema(name = "DomainRecordHistoryIdReq")
public class DomainRecordHistoryIdReq implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "主键ID")
    @NotNull(message = "ID不能为空")
    private Long id;

    public static DomainRecordHistoryIdReq of(Long id) {
        DomainRecordHistoryIdReq req = new DomainRecordHistoryIdReq();
        req.setId(id);
        return req;
    }
}
