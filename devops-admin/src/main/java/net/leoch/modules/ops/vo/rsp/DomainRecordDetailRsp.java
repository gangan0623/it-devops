package net.leoch.modules.ops.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.media.SchemaProperty;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * 域名记录详情响应
 */
@Data
@Schema(name = "DomainRecordDetailRsp")
public class DomainRecordDetailRsp extends DomainRecordRsp {

    @SchemaProperty(name = "应用交付")
    private DeliveryRsp delivery;

    @SchemaProperty(name = "内网解析")
    private DnsInternalRsp dnsInternal;

    @SchemaProperty(name = "外网解析")
    private DnsExternalRsp dnsExternal;

    @SchemaProperty(name = "防火墙映射")
    private FirewallMappingRsp firewallMapping;

    @Data
    @Schema(name = "DomainRecordDetailRsp.DeliveryRsp")
    public static class DeliveryRsp implements Serializable {
        @Serial
        private static final long serialVersionUID = 1L;

        @SchemaProperty(name = "主键ID")
        private Long id;

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

        @SchemaProperty(name = "备注")
        private String remark;

        @SchemaProperty(name = "节点明细")
        private List<DeliveryNodeRsp> nodes = new ArrayList<>();
    }

    @Data
    @Schema(name = "DomainRecordDetailRsp.DeliveryNodeRsp")
    public static class DeliveryNodeRsp implements Serializable {
        @Serial
        private static final long serialVersionUID = 1L;

        @SchemaProperty(name = "主键ID")
        private Long id;

        @SchemaProperty(name = "应用交付ID")
        private Long domainDeliveryId;

        @SchemaProperty(name = "节点IP")
        private String nodeIp;

        @SchemaProperty(name = "节点端口")
        private Integer nodePort;

        @SchemaProperty(name = "排序")
        private Integer sort;

        @SchemaProperty(name = "备注")
        private String remark;
    }

    @Data
    @Schema(name = "DomainRecordDetailRsp.DnsInternalRsp")
    public static class DnsInternalRsp implements Serializable {
        @Serial
        private static final long serialVersionUID = 1L;

        @SchemaProperty(name = "主键ID")
        private Long id;

        @SchemaProperty(name = "解析方式")
        private String resolveMode;

        @SchemaProperty(name = "DNS目标IP")
        private String dnsTargetIp;

        @SchemaProperty(name = "备注")
        private String remark;
    }

    @Data
    @Schema(name = "DomainRecordDetailRsp.DnsExternalRsp")
    public static class DnsExternalRsp implements Serializable {
        @Serial
        private static final long serialVersionUID = 1L;

        @SchemaProperty(name = "主键ID")
        private Long id;

        @SchemaProperty(name = "解析方式")
        private String resolveMode;

        @SchemaProperty(name = "DNS记录值")
        private String recordValue;

        @SchemaProperty(name = "备注")
        private String remark;
    }

    @Data
    @Schema(name = "DomainRecordDetailRsp.FirewallMappingRsp")
    public static class FirewallMappingRsp implements Serializable {
        @Serial
        private static final long serialVersionUID = 1L;

        @SchemaProperty(name = "主键ID")
        private Long id;

        @SchemaProperty(name = "公网IP")
        private String publicIp;

        @SchemaProperty(name = "外部端口")
        private Integer externalPort;

        @SchemaProperty(name = "内部IP")
        private String internalIp;

        @SchemaProperty(name = "内部端口")
        private Integer internalPort;

        @SchemaProperty(name = "映射描述")
        private String mappingDesc;
    }
}
