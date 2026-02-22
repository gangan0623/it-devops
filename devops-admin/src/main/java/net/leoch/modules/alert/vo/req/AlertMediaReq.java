package net.leoch.modules.alert.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * 告警媒介请求
 *
 * @author Taohongqiang
 */
@Data
@Schema(name = "告警媒介请求")
public class AlertMediaReq implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "媒介名称")
    private String name;

    @Schema(description = "SMTP Host")
    private String host;

    @Schema(description = "端口")
    private Integer port;

    @Schema(description = "用户名")
    private String username;

    @Schema(description = "密码")
    private String password;

    @Schema(description = "协议")
    private String protocol;

    @Schema(description = "SMTP认证")
    private Integer smtpAuth;

    @Schema(description = "STARTTLS")
    private Integer starttlsEnable;

    @Schema(description = "TLS/SSL")
    private Integer tlsEnable;

    @Schema(description = "发件人")
    private String fromAddr;

    @Schema(description = "状态 0禁用 1启用")
    private Integer status;
}
