package net.leoch.modules.ops.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.media.SchemaProperty;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.Date;

/**
 * 域名记录操作历史响应
 */
@Data
@Schema(name = "DomainRecordHistoryRsp")
public class DomainRecordHistoryRsp implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @SchemaProperty(name = "主键ID")
    private Long id;

    @SchemaProperty(name = "域名主记录ID")
    private Long domainRecordId;

    @SchemaProperty(name = "操作类型")
    private String operationType;

    @SchemaProperty(name = "操作人ID")
    private Long operatorId;

    @SchemaProperty(name = "操作人名称")
    private String operatorName;

    @SchemaProperty(name = "操作时间")
    private Date operationTime;

    @SchemaProperty(name = "操作摘要")
    private String operationSummary;
}
