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

    private Long id;

    private String name;

    private String ip;

    private String url;

    private Date backupTime;

    private Integer backupStatus;
}
