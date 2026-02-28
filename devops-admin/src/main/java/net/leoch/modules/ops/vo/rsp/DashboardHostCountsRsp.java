package net.leoch.modules.ops.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * 主机数量统计
 */
@Data
@Schema(name = "DashboardHostCountsRsp")
public class DashboardHostCountsRsp implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "资产总数(Windows+Linux+业务系统+网络设备)")
    private Long assetTotal;

    @Schema(description = "Windows主机数量")
    private Long windows;
    @Schema(description = "Windows在线数量")
    private Long windowsOnline;
    @Schema(description = "Windows离线数量")
    private Long windowsOffline;

    @Schema(description = "Linux主机数量")
    private Long linux;
    @Schema(description = "Linux在线数量")
    private Long linuxOnline;
    @Schema(description = "Linux离线数量")
    private Long linuxOffline;

    @Schema(description = "业务系统数量")
    private Long business;
    @Schema(description = "业务系统在线数量")
    private Long businessOnline;
    @Schema(description = "业务系统离线数量")
    private Long businessOffline;

    @Schema(description = "网络设备数量")
    private Long network;
    @Schema(description = "网络设备在线数量")
    private Long networkOnline;
    @Schema(description = "网络设备离线数量")
    private Long networkOffline;

    @Schema(description = "实体机数量")
    private Long physical;
    @Schema(description = "实体机在线数量")
    private Long physicalOnline;
    @Schema(description = "实体机离线数量")
    private Long physicalOffline;

    @Schema(description = "虚拟机数量")
    private Long vm;
    @Schema(description = "虚拟机在线数量")
    private Long vmOnline;
    @Schema(description = "虚拟机离线数量")
    private Long vmOffline;
}
