package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 监控组件ID请求
 */
@Data
@Schema(name = "MonitorComponentIdReq")
public class MonitorComponentIdReq implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "主键ID")
    private Long id;

    public static MonitorComponentIdReq of(Long id) {
        MonitorComponentIdReq req = new MonitorComponentIdReq();
        req.setId(id);
        return req;
    }
}
