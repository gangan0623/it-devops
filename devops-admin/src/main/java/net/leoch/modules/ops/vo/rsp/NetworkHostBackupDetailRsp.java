package net.leoch.modules.ops.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.Date;

@Data
@Schema(name = "NetworkHostBackupDetailRsp")
public class NetworkHostBackupDetailRsp implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    private Long networkHostId;
    private String instance;
    private String name;
    private String areaName;
    private String groupName;
    private String deviceModel;
    private Integer hostStatus;
    private Integer backupEnabled;
    private String username;
    private String password;
    private Long agentId;
    private String backupAgentName;
    private Integer canBackup;
    private Date lastBackupTime;
    private Integer lastBackupStatus;
    private String lastBackupMessage;
}

