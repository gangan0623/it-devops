package net.leoch.modules.ops.entity;

import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

/**
 * 域名记录字段差异明细表
 */
@Data
@TableName("tb_domain_record_history_detail")
public class DomainRecordHistoryDetailEntity {

    /**
     * 主键ID
     */
    private Long id;

    /**
     * 历史记录ID
     */
    private Long historyId;

    /**
     * 字段编码
     */
    private String fieldCode;

    /**
     * 字段名称
     */
    private String fieldName;

    /**
     * 修改前值
     */
    private String beforeValue;

    /**
     * 修改后值
     */
    private String afterValue;
}
