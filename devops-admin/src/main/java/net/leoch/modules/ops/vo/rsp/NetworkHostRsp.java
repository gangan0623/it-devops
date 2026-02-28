package net.leoch.modules.ops.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.Date;

@Data
@Schema(title = "网络设备")
public class NetworkHostRsp implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

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
    private Integer backupEnabled;
    private Integer backupStatus;
    private Long backupAgentId;
    private String backupAgentName;
    private Integer missingCount;
    private Date lastSeenTime;
    private Date lastSyncTime;
    private Long creator;
    private Date createDate;
    private Long updater;
    private Date updateDate;
}
