package net.leoch.modules.alert.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.util.Date;

/**
 * 告警记录操作历史
 */
@Data
@TableName("tb_alert_record_action")
public class AlertRecordActionEntity {

    /**
     * 主键ID
     */
    private Long id;

    /**
     * recordID
     */
    private Long recordId;

    /**
     * action
     */
    private String action;

    /**
     * 消息内容
     */
    private String message;

    /**
     * details
     */
    private String details;

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
