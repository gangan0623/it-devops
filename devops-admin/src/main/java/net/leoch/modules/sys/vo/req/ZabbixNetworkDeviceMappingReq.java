package net.leoch.modules.sys.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import net.leoch.modules.sys.vo.rsp.ZabbixNetworkDeviceMappingRsp;

import java.io.Serial;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Data
@Schema(title = "Zabbix网络设备映射配置请求")
public class ZabbixNetworkDeviceMappingReq implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    private String zabbixName;
    private List<String> selectedHostGroupIds = new ArrayList<>();
    private List<ZabbixNetworkDeviceMappingRsp.TemplateModelRule> templateModelRules = new ArrayList<>();
    private List<ZabbixNetworkDeviceMappingRsp.CategoryGroupRule> categoryGroupRules = new ArrayList<>();
    private List<ZabbixNetworkDeviceMappingRsp.AreaKeywordRule> areaKeywordRules = new ArrayList<>();
}
