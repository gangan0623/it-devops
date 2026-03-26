package net.leoch.modules.ops.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.media.SchemaProperty;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * 域名记录操作历史详情响应
 */
@Data
@Schema(name = "DomainRecordHistoryDetailRsp")
public class DomainRecordHistoryDetailRsp extends DomainRecordHistoryRsp {

    @SchemaProperty(name = "修改前快照")
    private String snapshotBefore;

    @SchemaProperty(name = "修改后快照")
    private String snapshotAfter;

    @SchemaProperty(name = "字段差异明细")
    private List<HistoryFieldDetailRsp> details = new ArrayList<>();

    @Data
    @Schema(name = "DomainRecordHistoryDetailRsp.HistoryFieldDetailRsp")
    public static class HistoryFieldDetailRsp implements Serializable {
        @Serial
        private static final long serialVersionUID = 1L;

        @SchemaProperty(name = "主键ID")
        private Long id;

        @SchemaProperty(name = "历史记录ID")
        private Long historyId;

        @SchemaProperty(name = "字段编码")
        private String fieldCode;

        @SchemaProperty(name = "字段名称")
        private String fieldName;

        @SchemaProperty(name = "修改前值")
        private String beforeValue;

        @SchemaProperty(name = "修改后值")
        private String afterValue;
    }
}
