package net.leoch.modules.ops.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.util.Date;

@Data
@TableName("tb_network_backup_device")
public class NetworkBackupDeviceEntity {
    private Long id;
    private Long networkHostId;
    private String instance;
    private String name;
    private String areaName;
    private String groupName;
    private String deviceModel;
    private Integer status;
    private String username;
    private String password;
    private Long agentId;
    private Integer backupEnabled;
    private Date lastBackupTime;
    private Integer lastBackupStatus;
    private String lastBackupMessage;

    @TableField(fill = FieldFill.INSERT)
    private Long creator;
    @TableField(fill = FieldFill.INSERT)
    private Date createDate;
    @TableField(fill = FieldFill.INSERT_UPDATE)
    private Long updater;
    @TableField(fill = FieldFill.INSERT_UPDATE)
    private Date updateDate;
}

