package net.leoch.modules.alert.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.media.SchemaProperty;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.Date;

/**
 * 告警媒介
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Data
@Schema(name = "告警媒介")
public class AlertMediaRsp implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @SchemaProperty(name = "ID")
    private Long id;

    @SchemaProperty(name = "媒介名称")
    private String name;

    @SchemaProperty(name = "SMTP Host")
    private String host;

    @SchemaProperty(name = "端口")
    private Integer port;

    @SchemaProperty(name = "用户名")
    private String username;

    @SchemaProperty(name = "密码")
    private String password;

	@SchemaProperty(name = "协议")
	private String protocol;

	@SchemaProperty(name = "SMTP认证")
	private Integer smtpAuth;

	@SchemaProperty(name = "STARTTLS")
	private Integer starttlsEnable;

	@SchemaProperty(name = "TLS/SSL")
	private Integer tlsEnable;

	@SchemaProperty(name = "发件人")
	private String fromAddr;

    @SchemaProperty(name = "状态 0禁用 1启用")
    private Integer status;

    @SchemaProperty(name = "创建者")
    private Long creator;

    @SchemaProperty(name = "创建时间")
    private Date createDate;

    @SchemaProperty(name = "更新者")
    private Long updater;

    @SchemaProperty(name = "更新时间")
    private Date updateDate;
}
