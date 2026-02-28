package net.leoch.modules.ops.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

@Data
@Schema(title = "网络设备接口详情")
public class NetworkInterfaceDetailRsp implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "接口状态")
    private String status;
    @Schema(description = "接口索引")
    private String interfaceIndex;
    @Schema(description = "接口名")
    private String interfaceName;
    @Schema(description = "接收流量（Kbps）")
    private Double inTrafficKbps;
    @Schema(description = "发送流量（Kbps）")
    private Double outTrafficKbps;
    @Schema(description = "端口速率（Mbps）")
    private Double speedMbps;
    @Schema(description = "进丢包/错包")
    private String inDropError;
    @Schema(description = "出丢包/错包")
    private String outDropError;
}
