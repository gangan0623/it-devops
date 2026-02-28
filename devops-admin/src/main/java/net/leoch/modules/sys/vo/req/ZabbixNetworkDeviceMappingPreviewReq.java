package net.leoch.modules.sys.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Data
@Schema(title = "Zabbix设备映射预览请求")
public class ZabbixNetworkDeviceMappingPreviewReq implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    private List<String> selectedHostGroupIds = new ArrayList<>();
    private List<net.leoch.modules.sys.vo.rsp.ZabbixNetworkDeviceMappingRsp.CategoryGroupRule> categoryGroupRules = new ArrayList<>();
    private List<net.leoch.modules.sys.vo.rsp.ZabbixNetworkDeviceMappingRsp.AreaKeywordRule> areaKeywordRules = new ArrayList<>();
}
