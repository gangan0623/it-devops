package net.leoch.modules.ops.entity;

import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.util.Date;

/**
 * 域名记录操作历史表
 */
@Data
@TableName("tb_domain_record_history")
public class DomainRecordHistoryEntity {

    /**
     * 主键ID
     */
    private Long id;

    /**
     * 域名主记录ID
     */
    private Long domainRecordId;

    /**
     * 操作类型
     */
    private String operationType;

    /**
     * 操作人ID
     */
    private Long operatorId;

    /**
     * 操作人名称
     */
    private String operatorName;

    /**
     * 操作时间
     */
    private Date operationTime;

    /**
     * 操作摘要
     */
    private String operationSummary;

    /**
     * 修改前快照
     */
    private String snapshotBefore;

    /**
     * 修改后快照
     */
    private String snapshotAfter;
}
