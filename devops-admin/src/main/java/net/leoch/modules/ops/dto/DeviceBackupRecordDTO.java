package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.media.SchemaProperty;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.Date;

/**
 * 设备备份信息表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-29
 */
@Data
@Schema(name = "设备备份信息表")
public class DeviceBackupRecordDTO implements Serializable {
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

    @SchemaProperty(name = "最后备份时间")
    private Date lastBackupTime;

    @SchemaProperty(name = "最后备份状态 1已完成 0异常")
    private Integer lastBackupStatus;

    @SchemaProperty(name = "备份次数")
    private Integer backupNum;
}
