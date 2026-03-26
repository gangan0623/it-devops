package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotEmpty;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * 域名记录删除请求
 */
@Data
@Schema(name = "DomainRecordDeleteReq")
public class DomainRecordDeleteReq implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID数组")
    @NotEmpty(message = "ID不能为空")
    private Long[] ids;

    public static DomainRecordDeleteReq of(Long[] ids) {
        DomainRecordDeleteReq req = new DomainRecordDeleteReq();
        req.setIds(ids);
        return req;
    }
}
