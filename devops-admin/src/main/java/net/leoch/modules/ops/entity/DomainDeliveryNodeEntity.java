package net.leoch.modules.ops.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.base.BaseEntity;

import java.util.Date;

/**
 * 域名应用交付节点明细表
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("tb_domain_delivery_node")
public class DomainDeliveryNodeEntity extends BaseEntity {

    /**
     * 应用交付ID
     */
    private Long domainDeliveryId;

    /**
     * 节点IP
     */
    private String nodeIp;

    /**
     * 节点端口
     */
    private Integer nodePort;

    /**
     * 排序
     */
    private Integer sort;

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
