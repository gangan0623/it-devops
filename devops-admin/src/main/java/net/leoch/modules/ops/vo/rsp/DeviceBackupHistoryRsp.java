package net.leoch.modules.ops.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.media.SchemaProperty;
import lombok.Data;

import java.io.Serializable;
import java.util.Date;

/**
 * 设备备份历史表
 */
@Data
@Schema(name = "设备备份历史表")
public class DeviceBackupHistoryRsp implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @SchemaProperty(name = "主键ID")
    private Long id;

    @SchemaProperty(name = "主机名")
    private String name;

    @SchemaProperty(name = "IP")
    private String ip;

    @SchemaProperty(name = "备份URL")
    private String url;

    @SchemaProperty(name = "备份时间")
    private Date backupTime;

    @SchemaProperty(name = "备份状态 1已完成 0异常")
    private Integer backupStatus;
}
