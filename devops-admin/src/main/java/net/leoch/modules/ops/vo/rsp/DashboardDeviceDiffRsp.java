package net.leoch.modules.ops.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;
import java.util.List;

/**
 * 设备差异
 */
@Data
@Schema(name = "DashboardDeviceDiffRsp")
public class DashboardDeviceDiffRsp implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "Zabbix存在但备份不存在")
    private List<DashboardDeviceDiffItemRsp> zabbixOnly;

    @Schema(description = "备份存在但Zabbix不存在")
    private List<DashboardDeviceDiffItemRsp> backupOnly;
}
