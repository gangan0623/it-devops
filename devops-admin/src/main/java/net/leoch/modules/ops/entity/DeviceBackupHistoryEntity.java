package net.leoch.modules.ops.entity;

import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.util.Date;

/**
 * 设备备份历史表
 */
@Data
@TableName("tb_device_backup_history")
public class DeviceBackupHistoryEntity {

    /**
     * 主键ID
     */
    private Long id;

    /**
     * 名称
     */
    private String name;

    /**
     * IP地址
     */
    private String ip;

    /**
     * URL地址
     */
    private String url;

    /**
     * backupTime时间
     */
    private Date backupTime;

    /**
     * 状态
     */
    private Integer backupStatus;
}
