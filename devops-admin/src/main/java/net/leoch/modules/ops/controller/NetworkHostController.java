package net.leoch.modules.ops.controller;

import cn.dev33.satoken.annotation.SaCheckPermission;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import net.leoch.common.base.Constant;
import net.leoch.common.data.page.PageData;
import net.leoch.common.data.result.Result;
import net.leoch.modules.ops.service.INetworkHostService;
import net.leoch.modules.ops.vo.req.NetworkHostBackupSaveReq;
import net.leoch.modules.ops.vo.req.NetworkHostPageReq;
import net.leoch.modules.ops.vo.rsp.NetworkInterfaceDetailRsp;
import net.leoch.modules.ops.vo.rsp.NetworkHostBackupDetailRsp;
import net.leoch.modules.ops.vo.rsp.NetworkHostRsp;
import net.leoch.modules.ops.vo.rsp.NetworkInterfaceTrendRsp;
import net.leoch.modules.ops.vo.rsp.OpsHostStatusSummaryRsp;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequestMapping("ops/networkhost")
@Tag(name = "网络设备管理")
@RequiredArgsConstructor
public class NetworkHostController {
    private final INetworkHostService networkHostService;

    @GetMapping("page")
    @Operation(summary = "分页")
    @Parameters({
            @Parameter(name = Constant.PAGE, description = "当前页码，从1开始", in = ParameterIn.QUERY, required = true, ref = "int"),
            @Parameter(name = Constant.LIMIT, description = "每页显示记录数", in = ParameterIn.QUERY, required = true, ref = "int"),
            @Parameter(name = Constant.ORDER_FIELD, description = "排序字段", in = ParameterIn.QUERY, ref = "String"),
            @Parameter(name = Constant.ORDER, description = "排序方式，可选值(asc、desc)", in = ParameterIn.QUERY, ref = "String")
    })
    @SaCheckPermission("ops:networkhost:page")
    public Result<PageData<NetworkHostRsp>> page(@Parameter(hidden = true) NetworkHostPageReq request) {
        return new Result<PageData<NetworkHostRsp>>().ok(networkHostService.page(request));
    }

    @GetMapping("summary")
    @Operation(summary = "状态汇总")
    @SaCheckPermission("ops:networkhost:page")
    public Result<OpsHostStatusSummaryRsp> summary(@Parameter(hidden = true) NetworkHostPageReq request) {
        return new Result<OpsHostStatusSummaryRsp>().ok(networkHostService.summary(request));
    }

    @GetMapping("backup/{networkHostId:\\d+}")
    @Operation(summary = "网络设备备份详情")
    @SaCheckPermission("ops:networkhost:page")
    public Result<NetworkHostBackupDetailRsp> backupDetail(@PathVariable Long networkHostId) {
        return new Result<NetworkHostBackupDetailRsp>().ok(networkHostService.backupDetail(networkHostId));
    }

    @PutMapping("backup")
    @Operation(summary = "保存网络设备备份配置")
    @SaCheckPermission("ops:networkhost:page")
    public Result<Object> saveBackupDetail(@RequestBody NetworkHostBackupSaveReq request) {
        networkHostService.saveBackupDetail(request);
        return new Result<>();
    }

    @GetMapping("interfaces")
    @Operation(summary = "按设备地址获取接口详情（实时调用Zabbix）")
    @SaCheckPermission("ops:networkhost:page")
    public Result<List<NetworkInterfaceDetailRsp>> interfaces(@RequestParam("instance") String instance,
                                                              @RequestParam(value = "includeZeroTraffic", defaultValue = "false") boolean includeZeroTraffic) {
        return new Result<List<NetworkInterfaceDetailRsp>>().ok(networkHostService.interfaceDetails(instance, includeZeroTraffic));
    }

    @GetMapping("interface-trend")
    @Operation(summary = "按设备地址与接口索引获取接口趋势（实时调用Zabbix）")
    @SaCheckPermission("ops:networkhost:page")
    public Result<NetworkInterfaceTrendRsp> interfaceTrend(@RequestParam("instance") String instance,
                                                           @RequestParam("interfaceIndex") String interfaceIndex,
                                                           @RequestParam("timeFrom") Long timeFrom,
                                                           @RequestParam("timeTill") Long timeTill) {
        return new Result<NetworkInterfaceTrendRsp>().ok(networkHostService.interfaceTrend(instance, interfaceIndex, timeFrom, timeTill));
    }
}
