package net.leoch.modules.ops.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.util.Date;

/**
 * 网络设备表（仅通过Zabbix同步维护）
 */
@Data
@TableName("tb_network_host")
public class NetworkHostEntity {
    private Long id;
    private String instance;
    private String name;
    private String areaName;
    private String groupName;
    private String deviceModel;
    private Integer status;
    private Integer collectionStatus;
    private Integer onlineStatus;
    private String responseTime;
    private String packetLossRate;
    private Integer missingCount;
    private Date lastSeenTime;
    private Date lastSyncTime;

    @TableField(fill = FieldFill.INSERT)
    private Long creator;
    @TableField(fill = FieldFill.INSERT)
    private Date createDate;
    @TableField(fill = FieldFill.INSERT_UPDATE)
    private Long updater;
    @TableField(fill = FieldFill.INSERT_UPDATE)
    private Date updateDate;
}
