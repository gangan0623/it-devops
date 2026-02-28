package net.leoch.modules.ops.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Data
@Schema(title = "Zabbix网络设备同步结果")
public class ZabbixNetworkHostSyncRsp implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    private Integer syncSuccess = 1;
    private String message = "OK";
    private Integer missingThreshold = 3;
    private Integer totalFetched = 0;
    private Integer inserted = 0;
    private Integer updated = 0;
    private Integer disabled = 0;
    @Schema(description = "逻辑删除(禁用)数量")
    private Integer logicalDeleted = 0;
    private Integer skipped = 0;
    private List<String> unmatchedAreas = new ArrayList<>();
}
