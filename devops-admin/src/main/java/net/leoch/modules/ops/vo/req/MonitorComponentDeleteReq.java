package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 监控组件删除请求
 */
@Data
@Schema(name = "MonitorComponentDeleteReq")
public class MonitorComponentDeleteReq implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID数组")
    private Long[] ids;

    public static MonitorComponentDeleteReq of(Long[] ids) {
        MonitorComponentDeleteReq req = new MonitorComponentDeleteReq();
        req.setIds(ids);
        return req;
    }
}
