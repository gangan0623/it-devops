package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 监控组件ID请求
 */
@Data
@Schema(name = "MonitorComponentIdRequest")
public class MonitorComponentIdRequest implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "主键ID")
    private Long id;

    public static MonitorComponentIdRequest of(Long id) {
        MonitorComponentIdRequest req = new MonitorComponentIdRequest();
        req.setId(id);
        return req;
    }
}
