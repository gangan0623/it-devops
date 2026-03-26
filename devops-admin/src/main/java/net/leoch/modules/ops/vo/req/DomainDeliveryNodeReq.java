package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * 域名应用交付节点请求
 */
@Data
@Schema(name = "DomainDeliveryNodeReq")
public class DomainDeliveryNodeReq implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "主键ID")
    private Long id;

    @Schema(description = "应用交付ID")
    private Long domainDeliveryId;

    @Schema(description = "节点IP")
    @NotBlank(message = "节点IP不能为空")
    private String nodeIp;

    @Schema(description = "节点端口")
    @NotNull(message = "节点端口不能为空")
    @Min(value = 1, message = "节点端口取值不合法")
    @Max(value = 65535, message = "节点端口取值不合法")
    private Integer nodePort;

    @Schema(description = "排序")
    private Integer sort;

    @Schema(description = "备注")
    private String remark;
}
