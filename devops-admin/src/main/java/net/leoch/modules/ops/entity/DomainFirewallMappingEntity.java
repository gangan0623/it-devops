package net.leoch.modules.ops.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.base.BaseEntity;

import java.util.Date;

/**
 * 域名防火墙映射表
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("tb_domain_firewall_mapping")
public class DomainFirewallMappingEntity extends BaseEntity {

    /**
     * 域名主记录ID
     */
    private Long domainRecordId;

    /**
     * 公网IP
     */
    private String publicIp;

    /**
     * 外部端口
     */
    private Integer externalPort;

    /**
     * 内部IP
     */
    private String internalIp;

    /**
     * 内部端口
     */
    private Integer internalPort;

    /**
     * 映射描述
     */
    private String mappingDesc;

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
