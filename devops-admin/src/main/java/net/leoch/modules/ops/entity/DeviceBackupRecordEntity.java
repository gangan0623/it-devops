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

    private Long id;

    private String name;

    private String ip;

    private String url;

    private Date lastBackupTime;

    private Integer lastBackupStatus;

    private Integer backupNum;
}
