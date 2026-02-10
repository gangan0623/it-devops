package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * 监控组件列表请求
 */
@Data
@Schema(name = "MonitorComponentListReq")
public class MonitorComponentListReq implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

}
