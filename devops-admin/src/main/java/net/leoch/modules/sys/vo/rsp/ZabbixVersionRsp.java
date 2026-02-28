package net.leoch.modules.sys.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

@Data
@Schema(title = "Zabbix版本检测结果")
public class ZabbixVersionRsp implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "当前版本")
    private String currentVersion;

    @Schema(description = "最新版本")
    private String latestVersion;

    @Schema(description = "是否可更新 0否 1是")
    private Integer updateAvailable;

    @Schema(description = "更新地址")
    private String upgradeUrl;
}
