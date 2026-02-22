package net.leoch.modules.alert.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.util.Date;

/**
 * 告警媒介
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Data
@TableName("tb_alert_media")
public class AlertMediaEntity {

    /**
     * 主键ID
     */
    private Long id;

    /**
     * 媒介名称
     */
    private String name;

    /**
     * 邮件服务器地址
     */
    private String host;

    /**
     * 邮件服务器端口
     */
    private Integer port;

    /**
     * 用户名
     */
    private String username;

    /**
     * 密码
     */
    private String password;

    /**
     * 协议（smtp/smtps）
     */
    private String protocol;

    /**
     * 是否启用SMTP认证（0否 1是）
     */
    private Integer smtpAuth;

    /**
     * 是否启用STARTTLS（0否 1是）
     */
    private Integer starttlsEnable;

    /**
     * 是否启用TLS（0否 1是）
     */
    private Integer tlsEnable;

    /**
     * 发件人地址
     */
    private String fromAddr;

    /**
     * 状态（0停用 1启用）
     */
    private Integer status;

    /**
     * 创建者ID
     */
    @TableField(fill = FieldFill.INSERT)
    private Long creator;

    /**
     * 创建时间
     */
    @TableField(fill = FieldFill.INSERT)
    private Date createDate;

    /**
     * 更新者ID
     */
    @TableField(fill = FieldFill.INSERT_UPDATE)
    private Long updater;

    /**
     * 更新时间
     */
    @TableField(fill = FieldFill.INSERT_UPDATE)
    private Date updateDate;
}
