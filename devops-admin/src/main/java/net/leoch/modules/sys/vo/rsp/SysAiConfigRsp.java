package net.leoch.modules.sys.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * AI配置响应
 */
@Data
@Schema(title = "AI配置响应")
public class SysAiConfigRsp implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    private String baseUrl;

    private String apiKey;

    private String model;

    private Integer status;
}
