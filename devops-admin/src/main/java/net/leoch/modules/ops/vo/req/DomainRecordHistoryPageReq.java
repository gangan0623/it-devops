package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.data.page.BasePage;

import java.util.Date;

/**
 * 域名记录操作历史分页请求
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(name = "DomainRecordHistoryPageReq")
public class DomainRecordHistoryPageReq extends BasePage {

    @Schema(description = "域名主记录ID")
    private Long domainRecordId;

    @Schema(description = "操作类型")
    private String operationType;

    @Schema(description = "操作人名称")
    private String operatorName;

    @Schema(description = "操作时间开始")
    private Date operationTimeStart;

    @Schema(description = "操作时间结束")
    private Date operationTimeEnd;
}
