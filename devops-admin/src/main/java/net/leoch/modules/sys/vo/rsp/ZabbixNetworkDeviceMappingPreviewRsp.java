package net.leoch.modules.sys.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Data
@Schema(title = "Zabbix设备映射预览")
public class ZabbixNetworkDeviceMappingPreviewRsp implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    private List<GroupPreviewItem> groupList = new ArrayList<>();
    private List<String> unmatchedAreas = new ArrayList<>();

    @Data
    public static class GroupPreviewItem implements Serializable {
        @Serial
        private static final long serialVersionUID = 1L;
        private String groupId;
        private String groupName;
        private String zabbixCategory;
        private String rawAreaSegment;
        private String normalizedAreaKeyword;
        private String matchedAreaName;
        private String mappedDeviceGroup;
        private Integer matched = 0;
    }
}
