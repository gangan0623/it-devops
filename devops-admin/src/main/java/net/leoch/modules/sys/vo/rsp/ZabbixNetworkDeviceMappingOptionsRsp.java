package net.leoch.modules.sys.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Data
@Schema(title = "Zabbix网络设备映射选项")
public class ZabbixNetworkDeviceMappingOptionsRsp implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    private String zabbixName;
    private List<String> templateOptions = new ArrayList<>();
    private List<ZabbixHostGroupRsp> hostGroupOptions = new ArrayList<>();
    private List<DictDataRsp> areaOptions = new ArrayList<>();
    private List<DictDataRsp> deviceGroupOptions = new ArrayList<>();
    private List<DictDataRsp> deviceModelOptions = new ArrayList<>();
}
