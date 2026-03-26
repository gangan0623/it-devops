package net.leoch.modules.ops.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.media.SchemaProperty;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.Date;

/**
 * 域名记录列表响应
 */
@Data
@Schema(name = "DomainRecordRsp")
public class DomainRecordRsp implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @SchemaProperty(name = "主键ID")
    private Long id;

    @SchemaProperty(name = "项目名称")
    private String projectName;

    @SchemaProperty(name = "域名")
    private String domainName;

    @SchemaProperty(name = "是否走应用交付 0否 1是")
    private Integer adEnabled;

    @SchemaProperty(name = "是否启用内网解析 0否 1是")
    private Integer internalEnabled;

    @SchemaProperty(name = "是否启用外网解析 0否 1是")
    private Integer externalEnabled;

    @SchemaProperty(name = "外网访问地址")
    private String externalAddress;

    @SchemaProperty(name = "描述")
    private String description;

    @SchemaProperty(name = "项目负责人")
    private String projectOwner;

    @SchemaProperty(name = "申请时间")
    private Date applyTime;

    @SchemaProperty(name = "备注")
    private String remark;

    @SchemaProperty(name = "虚拟服务名称")
    private String virtualServiceName;

    @SchemaProperty(name = "虚拟服务IP")
    private String virtualServiceIp;

    @SchemaProperty(name = "虚拟服务端口")
    private Integer virtualServicePort;

    @SchemaProperty(name = "虚拟服务协议")
    private String virtualServiceProtocol;

    @SchemaProperty(name = "节点池名称")
    private String poolName;

    @SchemaProperty(name = "负载策略")
    private String loadStrategy;

    @SchemaProperty(name = "创建者")
    private Long creator;

    @SchemaProperty(name = "创建时间")
    private Date createDate;

    @SchemaProperty(name = "更新者")
    private Long updater;

    @SchemaProperty(name = "更新时间")
    private Date updateDate;
}
