package net.leoch.modules.alert.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.util.Date;

/**
 * 告警记录
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Data
@TableName("tb_alert_record")
public class AlertRecordEntity {

    /**
     * 主键ID
     */
    private Long id;

    /**
     * 名称
     */
    private String alertName;

    /**
     * 状态（0停用 1启用）
     */
    private String status;

    /**
     * 严重程度
     */
    private String severity;

    /**
     * instance
     */
    private String instance;

    /**
     * summary
     */
    private String summary;

    /**
     * 描述
     */
    private String description;

    /**
     * startsAt
     */
    private Date startsAt;

    /**
     * endsAt
     */
    private Date endsAt;

    /**
     * receiver
     */
    private String receiver;

    /**
     * 告警组(alertgroup标签)
     */
    private String alertGroup;

    /**
     * Alertmanager fingerprint
     */
    private String fingerprint;

    /**
     * closed
     */
    private Integer closed;

    /**
     * suppressedUntil
     */
    private Date suppressedUntil;

    /**
     * 首次发现时间
     */
    private Date firstSeenAt;

    /**
     * 最后收到Webhook时间
     */
    private Date lastSeenAt;

    /**
     * 最后通知时间
     */
    private Date lastNotifiedAt;

    /**
     * 通知次数
     */
    private Integer notifyCount;

    @TableField(fill = FieldFill.INSERT)
    /**
     * 创建者ID
     */
    private Long creator;

    @TableField(fill = FieldFill.INSERT)
    /**
     * 创建时间
     */
    private Date createDate;

    @TableField(fill = FieldFill.INSERT_UPDATE)
    /**
     * 更新者ID
     */
    private Long updater;

    @TableField(fill = FieldFill.INSERT_UPDATE)
    /**
     * 更新时间
     */
    private Date updateDate;
}
