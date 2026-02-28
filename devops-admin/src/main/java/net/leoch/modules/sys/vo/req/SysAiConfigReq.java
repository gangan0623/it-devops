package net.leoch.modules.sys.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;
import org.hibernate.validator.constraints.Range;

import java.io.Serial;
import java.io.Serializable;

/**
 * AI配置请求
 */
@Data
@Schema(title = "AI配置请求")
public class SysAiConfigReq implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @NotBlank(message = "Base URL不能为空")
    @Schema(title = "Base URL")
    private String baseUrl;

    @NotBlank(message = "API Key不能为空")
    @Schema(title = "API Key")
    private String apiKey;

    @NotBlank(message = "Model不能为空")
    @Schema(title = "Model")
    private String model;

    @Range(min = 0, max = 1, message = "状态值非法")
    @Schema(title = "状态 0禁用 1启用")
    private Integer status;
}
