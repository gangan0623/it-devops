package net.leoch.modules.ops.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.base.BaseEntity;

import java.util.Date;

/**
 * 域名内网解析表
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("tb_domain_dns_internal")
public class DomainDnsInternalEntity extends BaseEntity {

    /**
     * 域名主记录ID
     */
    private Long domainRecordId;

    /**
     * 解析方式
     */
    private String resolveMode;

    /**
     * DNS目标IP
     */
    private String dnsTargetIp;

    /**
     * 备注
     */
    private String remark;

    /**
     * 更新者
     */
    @TableField(fill = FieldFill.INSERT_UPDATE)
    private Long updater;

    /**
     * 更新时间
     */
    @TableField(fill = FieldFill.INSERT_UPDATE)
    private Date updateDate;
}
