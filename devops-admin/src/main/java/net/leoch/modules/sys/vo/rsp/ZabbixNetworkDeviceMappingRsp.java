package net.leoch.modules.sys.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Data
@Schema(title = "Zabbix网络设备映射配置")
public class ZabbixNetworkDeviceMappingRsp implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "Zabbix名称")
    private String zabbixName;

    @Schema(description = "已选择主机群组ID")
    private List<String> selectedHostGroupIds = new ArrayList<>();

    @Schema(description = "模板到设备型号映射")
    private List<TemplateModelRule> templateModelRules = new ArrayList<>();

    @Schema(description = "Zabbix分类到设备分组映射")
    private List<CategoryGroupRule> categoryGroupRules = new ArrayList<>();

    @Schema(description = "区域关键字映射（归一化）")
    private List<AreaKeywordRule> areaKeywordRules = new ArrayList<>();

    @Data
    public static class TemplateModelRule implements Serializable {
        @Serial
        private static final long serialVersionUID = 1L;
        private String templateName;
        private String deviceModel;
    }

    @Data
    public static class CategoryGroupRule implements Serializable {
        @Serial
        private static final long serialVersionUID = 1L;
        private String zabbixCategory;
        private String deviceGroup;
    }

    @Data
    public static class AreaKeywordRule implements Serializable {
        @Serial
        private static final long serialVersionUID = 1L;
        private String keyword;
        private String areaName;
    }
}
