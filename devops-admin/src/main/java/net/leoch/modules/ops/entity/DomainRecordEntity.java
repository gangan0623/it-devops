package net.leoch.modules.ops.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.base.BaseEntity;

import java.util.Date;

/**
 * 域名主记录表
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("tb_domain_record")
public class DomainRecordEntity extends BaseEntity {

    /**
     * 是否走应用交付 0否 1是
     */
    private Integer adEnabled;

    /**
     * 是否启用内网解析 0否 1是
     */
    private Integer internalEnabled;

    /**
     * 是否启用外网解析 0否 1是
     */
    private Integer externalEnabled;

    /**
     * 外网访问地址
     */
    private String externalAddress;

    /**
     * 区域名称
     */
    private String areaName;

    /**
     * 项目名称
     */
    private String projectName;

    /**
     * 域名
     */
    private String domainName;

    /**
     * 描述
     */
    private String description;

    /**
     * 项目负责人
     */
    private String projectOwner;

    /**
     * 申请时间
     */
    private Date applyTime;

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
