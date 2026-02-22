package net.leoch.modules.ops.entity;

import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.util.Date;

/**
 * 设备备份信息表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-29
 */
@Data
@TableName("tb_device_backup_record")
public class DeviceBackupRecordEntity {

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
     * lastBackupTime时间
     */
    private Date lastBackupTime;

    /**
     * 状态
     */
    private Integer lastBackupStatus;

    /**
     * backupNum
     */
    private Integer backupNum;
}
