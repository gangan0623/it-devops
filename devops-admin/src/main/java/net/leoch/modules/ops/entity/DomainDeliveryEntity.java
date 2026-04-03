package net.leoch.modules.ops.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.base.BaseEntity;

import java.util.Date;

/**
 * 域名应用交付表
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("tb_domain_delivery")
public class DomainDeliveryEntity extends BaseEntity {

    /**
     * 域名主记录ID
     */
    private Long domainRecordId;

    /**
     * 虚拟服务名称
     */
    private String virtualServiceName;

    /**
     * 虚拟服务IP
     */
    private String virtualServiceIp;

    /**
     * 虚拟服务端口
     */
    private Integer virtualServicePort;

    /**
     * 虚拟服务协议
     */
    private String virtualServiceProtocol;

    /**
     * 节点池名称
     */
    private String poolName;

    /**
     * 外网虚拟服务名称
     */
    private String externalVirtualServiceName;

    /**
     * 外网虚拟服务IP
     */
    private String externalVirtualServiceIp;

    /**
     * 外网虚拟服务端口
     */
    private Integer externalVirtualServicePort;

    /**
     * 外网虚拟服务协议
     */
    private String externalVirtualServiceProtocol;

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
