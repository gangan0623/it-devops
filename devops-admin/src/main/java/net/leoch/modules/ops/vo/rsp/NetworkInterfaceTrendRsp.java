package net.leoch.modules.ops.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.List;

@Data
@Schema(title = "网络设备接口趋势")
public class NetworkInterfaceTrendRsp implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "接口索引")
    private String interfaceIndex;
    @Schema(description = "时间戳(秒)")
    private List<Long> timestamps;
    @Schema(description = "入站比特率(Kbps)")
    private List<Double> inBitrateKbps;
    @Schema(description = "出站比特率(Kbps)")
    private List<Double> outBitrateKbps;
}
