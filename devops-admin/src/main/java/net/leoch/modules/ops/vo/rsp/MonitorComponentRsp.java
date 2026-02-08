package net.leoch.modules.ops.vo.rsp;

import io.swagger.v3.oas.annotations.media.SchemaProperty;
import lombok.Data;

import java.io.Serializable;
import java.util.Date;

/**
 * 监控组件
 */
@Data
@io.swagger.v3.oas.annotations.media.Schema(name = "监控组件")
public class MonitorComponentRsp implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @SchemaProperty(name = "主键ID")
    private Long id;

    @SchemaProperty(name = "名称")
    private String name;

    @SchemaProperty(name = "类型")
    private String type;

    @SchemaProperty(name = "IP")
    private String ip;

    @SchemaProperty(name = "端口")
    private Integer port;

    @SchemaProperty(name = "Web地址")
    private String webUrl;

    @SchemaProperty(name = "在线状态 0离线 1在线")
    private Integer onlineStatus;

    @SchemaProperty(name = "当前版本")
    private String version;

    @SchemaProperty(name = "最新版本")
    private String latestVersion;

    @SchemaProperty(name = "是否可更新 0否 1是")
    private Integer updateAvailable;

    @SchemaProperty(name = "最后检测时间")
    private Date lastCheckTime;

    @SchemaProperty(name = "创建者")
    private Long creator;

    @SchemaProperty(name = "创建时间")
    private Date createDate;

    @SchemaProperty(name = "更新者")
    private Long updater;

    @SchemaProperty(name = "更新时间")
    private Date updateDate;
}
