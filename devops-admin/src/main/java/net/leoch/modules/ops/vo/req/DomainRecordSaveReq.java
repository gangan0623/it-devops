package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.Valid;
import jakarta.validation.constraints.AssertTrue;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Null;
import jakarta.validation.constraints.Pattern;
import lombok.Data;
import net.leoch.common.data.validator.group.AddGroup;
import net.leoch.common.data.validator.group.DefaultGroup;

import java.io.Serial;
import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * 域名记录保存请求
 */
@Data
@Schema(name = "DomainRecordSaveReq")
public class DomainRecordSaveReq implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    private static final String DOMAIN_PATTERN = "^(?=.{1,253}$)(?:(?!-)[A-Za-z0-9-]{1,63}(?<!-)\\.)+[A-Za-z]{2,63}$";

    @Schema(title = "id")
    @Null(message = "{id.null}", groups = AddGroup.class)
    private Long id;

    @Schema(title = "项目名称")
    @NotBlank(message = "项目名称不能为空", groups = DefaultGroup.class)
    private String projectName;

    @Schema(title = "域名")
    @NotBlank(message = "域名不能为空", groups = DefaultGroup.class)
    @Pattern(regexp = DOMAIN_PATTERN, message = "域名格式不合法", groups = DefaultGroup.class)
    private String domainName;

    @Schema(title = "是否走应用交付 0否 1是")
    @NotNull(message = "是否走应用交付不能为空", groups = DefaultGroup.class)
    @Min(value = 0, message = "是否走应用交付取值不合法", groups = DefaultGroup.class)
    @Max(value = 1, message = "是否走应用交付取值不合法", groups = DefaultGroup.class)
    private Integer adEnabled;

    @Schema(title = "是否启用内网解析 0否 1是")
    @NotNull(message = "是否启用内网解析不能为空", groups = DefaultGroup.class)
    @Min(value = 0, message = "是否启用内网解析取值不合法", groups = DefaultGroup.class)
    @Max(value = 1, message = "是否启用内网解析取值不合法", groups = DefaultGroup.class)
    private Integer internalEnabled;

    @Schema(title = "是否启用外网解析 0否 1是")
    @NotNull(message = "是否启用外网解析不能为空", groups = DefaultGroup.class)
    @Min(value = 0, message = "是否启用外网解析取值不合法", groups = DefaultGroup.class)
    @Max(value = 1, message = "是否启用外网解析取值不合法", groups = DefaultGroup.class)
    private Integer externalEnabled;

    @Schema(title = "外网访问地址")
    private String externalAddress;

    @Schema(title = "描述")
    private String description;

    @Schema(title = "项目负责人")
    @NotBlank(message = "项目负责人不能为空", groups = DefaultGroup.class)
    private String projectOwner;

    @Schema(title = "申请时间")
    @NotNull(message = "申请时间不能为空", groups = DefaultGroup.class)
    private Date applyTime;

    @Schema(title = "备注")
    private String remark;

    @Schema(title = "应用交付")
    @Valid
    private DeliveryReq delivery;

    @Schema(title = "内网解析")
    @Valid
    private DnsInternalReq dnsInternal;

    @Schema(title = "外网解析")
    @Valid
    private DnsExternalReq dnsExternal;

    @Schema(title = "防火墙映射")
    @Valid
    private FirewallMappingReq firewallMapping;

    @AssertTrue(message = "启用应用交付时必须填写应用交付信息", groups = DefaultGroup.class)
    @Schema(hidden = true)
    public boolean isDeliveryValid() {
        return !Integer.valueOf(1).equals(adEnabled) || delivery != null;
    }

    @AssertTrue(message = "启用内网解析时必须填写内网解析信息", groups = DefaultGroup.class)
    @Schema(hidden = true)
    public boolean isDnsInternalValid() {
        return !Integer.valueOf(1).equals(internalEnabled) || dnsInternal != null;
    }

    @AssertTrue(message = "启用外网解析时必须填写外网解析信息", groups = DefaultGroup.class)
    @Schema(hidden = true)
    public boolean isDnsExternalValid() {
        return !Integer.valueOf(1).equals(externalEnabled) || dnsExternal != null;
    }

    @AssertTrue(message = "启用应用交付且启用外网解析时必须填写防火墙映射信息", groups = DefaultGroup.class)
    @Schema(hidden = true)
    public boolean isFirewallMappingValid() {
        return !(Integer.valueOf(1).equals(adEnabled) && Integer.valueOf(1).equals(externalEnabled))
            || firewallMapping != null;
    }

    @Data
    @Schema(name = "DomainRecordSaveReq.DeliveryReq")
    public static class DeliveryReq implements Serializable {
        @Serial
        private static final long serialVersionUID = 1L;

        @Schema(title = "虚拟服务名称")
        @NotBlank(message = "虚拟服务名称不能为空", groups = DefaultGroup.class)
        private String virtualServiceName;

        @Schema(title = "虚拟服务IP")
        @NotBlank(message = "虚拟服务IP不能为空", groups = DefaultGroup.class)
        private String virtualServiceIp;

        @Schema(title = "虚拟服务端口")
        @NotNull(message = "虚拟服务端口不能为空", groups = DefaultGroup.class)
        @Min(value = 1, message = "虚拟服务端口取值不合法", groups = DefaultGroup.class)
        @Max(value = 65535, message = "虚拟服务端口取值不合法", groups = DefaultGroup.class)
        private Integer virtualServicePort;

        @Schema(title = "虚拟服务协议")
        @NotBlank(message = "虚拟服务协议不能为空", groups = DefaultGroup.class)
        @Pattern(regexp = "^(HTTP|HTTPS|TCP)$", message = "虚拟服务协议取值不合法", groups = DefaultGroup.class)
        private String virtualServiceProtocol;

        @Schema(title = "节点池名称")
        @NotBlank(message = "节点池名称不能为空", groups = DefaultGroup.class)
        private String poolName;

        @Schema(title = "负载策略")
        @NotBlank(message = "负载策略不能为空", groups = DefaultGroup.class)
        private String loadStrategy;

        @Schema(title = "备注")
        private String remark;

        @Schema(title = "节点明细")
        @Valid
        private List<DomainDeliveryNodeReq> nodes;
    }

    @Data
    @Schema(name = "DomainRecordSaveReq.DnsInternalReq")
    public static class DnsInternalReq implements Serializable {
        @Serial
        private static final long serialVersionUID = 1L;

        @Schema(title = "解析方式")
        @NotBlank(message = "解析方式不能为空", groups = DefaultGroup.class)
        @Pattern(regexp = "^(AD|DIRECT)$", message = "解析方式取值不合法", groups = DefaultGroup.class)
        private String resolveMode;

        @Schema(title = "DNS目标IP")
        @NotBlank(message = "DNS目标IP不能为空", groups = DefaultGroup.class)
        private String dnsTargetIp;

        @Schema(title = "备注")
        private String remark;
    }

    @Data
    @Schema(name = "DomainRecordSaveReq.DnsExternalReq")
    public static class DnsExternalReq implements Serializable {
        @Serial
        private static final long serialVersionUID = 1L;

        @Schema(title = "解析方式")
        @NotBlank(message = "解析方式不能为空", groups = DefaultGroup.class)
        @Pattern(regexp = "^(AD|DIRECT)$", message = "解析方式取值不合法", groups = DefaultGroup.class)
        private String resolveMode;

        @Schema(title = "DNS记录值")
        @NotBlank(message = "DNS记录值不能为空", groups = DefaultGroup.class)
        private String recordValue;

        @Schema(title = "备注")
        private String remark;
    }

    @Data
    @Schema(name = "DomainRecordSaveReq.FirewallMappingReq")
    public static class FirewallMappingReq implements Serializable {
        @Serial
        private static final long serialVersionUID = 1L;

        @Schema(title = "公网IP")
        @NotBlank(message = "公网IP不能为空", groups = DefaultGroup.class)
        private String publicIp;

        @Schema(title = "外部端口")
        @NotNull(message = "外部端口不能为空", groups = DefaultGroup.class)
        @Min(value = 1, message = "外部端口取值不合法", groups = DefaultGroup.class)
        @Max(value = 65535, message = "外部端口取值不合法", groups = DefaultGroup.class)
        private Integer externalPort;

        @Schema(title = "内部IP")
        @NotBlank(message = "内部IP不能为空", groups = DefaultGroup.class)
        private String internalIp;

        @Schema(title = "内部端口")
        @NotNull(message = "内部端口不能为空", groups = DefaultGroup.class)
        @Min(value = 1, message = "内部端口取值不合法", groups = DefaultGroup.class)
        @Max(value = 65535, message = "内部端口取值不合法", groups = DefaultGroup.class)
        private Integer internalPort;

        @Schema(title = "映射描述")
        private String mappingDesc;
    }
}
