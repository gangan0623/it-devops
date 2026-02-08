package net.leoch.modules.ops.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.util.Date;

/**
 * 监控组件
 */
@Data
@TableName("tb_monitor_component")
public class MonitorComponentEntity {
    /**
     * 主键ID
     */
    private Long id;
    /**
     * 名称
     */
    private String name;
    /**
     * 类型
     */
    private String type;
    /**
     * IP
     */
    private String ip;
    /**
     * 端口
     */
    private Integer port;
    /**
     * Web地址
     */
    private String webUrl;
    /**
     * 在线状态 0离线 1在线
     */
    private Integer onlineStatus;
    /**
     * 当前版本
     */
    private String version;
    /**
     * 最新版本
     */
    private String latestVersion;
    /**
     * 是否可更新 0否 1是
     */
    private Integer updateAvailable;
    /**
     * 最后检测时间
     */
    private Date lastCheckTime;
    /**
     * 创建者
     */
    @TableField(fill = FieldFill.INSERT)
    private Long creator;
    /**
     * 创建时间
     */
    @TableField(fill = FieldFill.INSERT)
    private Date createDate;
    /**
     * 更新者
     */
    @TableField(fill = FieldFill.INSERT_UPDATE)
    private Long updater;
    /**
     * 更新时间
     */
    @TableField(fill = FieldFill.INSERT_UPDATE)
    private Date updateDate;
}
