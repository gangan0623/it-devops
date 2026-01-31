package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 监控组件列表请求
 */
@Data
@Schema(name = "MonitorComponentListRequest")
public class MonitorComponentListRequest implements Serializable {
    private static final long serialVersionUID = 1L;

}
