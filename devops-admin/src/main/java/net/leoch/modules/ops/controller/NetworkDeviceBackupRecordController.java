package net.leoch.modules.ops.controller;


import cn.dev33.satoken.annotation.SaCheckPermission;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import net.leoch.common.annotation.LogOperation;
import net.leoch.common.base.Constant;
import net.leoch.common.data.page.PageData;
import net.leoch.common.data.result.Result;
import net.leoch.modules.ops.service.INetworkDeviceBackupRecordService;
import net.leoch.modules.ops.vo.req.*;
import net.leoch.modules.ops.vo.rsp.NetworkDeviceBackupDiffLineRsp;
import net.leoch.modules.ops.vo.rsp.NetworkDeviceBackupHistoryRsp;
import net.leoch.modules.ops.vo.rsp.NetworkDeviceBackupRecordRsp;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 网络设备备份记录
 */
@RestController
@RequestMapping({"ops/devicebackuprecord", "ops/network-device-backup-record"})
@Tag(name="网络设备备份记录")
@RequiredArgsConstructor
public class NetworkDeviceBackupRecordController {

    private final INetworkDeviceBackupRecordService deviceBackupRecordService;

    @GetMapping("page")
    @Operation(summary = "网络设备备份记录分页")
    @Parameters({
        @Parameter(name = Constant.PAGE, description = "当前页码，从1开始", in = ParameterIn.QUERY, required = true, ref="int") ,
        @Parameter(name = Constant.LIMIT, description = "每页显示记录数", in = ParameterIn.QUERY,required = true, ref="int") ,
        @Parameter(name = Constant.ORDER_FIELD, description = "排序字段", in = ParameterIn.QUERY, ref="String") ,
        @Parameter(name = Constant.ORDER, description = "排序方式，可选值(asc、desc)", in = ParameterIn.QUERY, ref="String")
    })
    @SaCheckPermission("ops:network-device-backup-record:page")
    public Result<PageData<NetworkDeviceBackupRecordRsp>> page(@Parameter(hidden = true) NetworkDeviceBackupRecordPageReq request){
        return new Result<PageData<NetworkDeviceBackupRecordRsp>>().ok(deviceBackupRecordService.page(request));
    }

    @GetMapping("{id}")
    @Operation(summary = "网络设备备份记录信息")
    @SaCheckPermission("ops:network-device-backup-record:info")
    public Result<NetworkDeviceBackupRecordRsp> get(@PathVariable Long id){
        return new Result<NetworkDeviceBackupRecordRsp>().ok(deviceBackupRecordService.get(NetworkDeviceBackupRecordIdReq.of(id)));
    }

    @DeleteMapping
    @Operation(summary = "删除网络设备备份记录")
    @LogOperation("删除")
    @SaCheckPermission("ops:network-device-backup-record:delete")
    public Result<Object> delete(@RequestBody Long[] ids){
        deviceBackupRecordService.delete(NetworkDeviceBackupRecordDeleteReq.of(ids));
        return new Result<>();
    }

    @GetMapping("history")
    @Operation(summary = "网络设备备份历史列表")
    @SaCheckPermission("ops:network-device-backup-record:history")
    public Result<List<NetworkDeviceBackupHistoryRsp>> history(NetworkDeviceBackupRecordHistoryReq request) {
        return new Result<List<NetworkDeviceBackupHistoryRsp>>().ok(deviceBackupRecordService.history(request));
    }

    @GetMapping("diff")
    @Operation(summary = "网络设备备份历史对比")
    @SaCheckPermission("ops:network-device-backup-record:diff")
    public Result<List<NetworkDeviceBackupDiffLineRsp>> diff(NetworkDeviceBackupRecordDiffReq request) {
        return new Result<List<NetworkDeviceBackupDiffLineRsp>>().ok(deviceBackupRecordService.diff(request));
    }

    @GetMapping("diff-current")
    @Operation(summary = "网络设备备份历史对比当前")
    @SaCheckPermission("ops:network-device-backup-record:diff")
    public Result<List<NetworkDeviceBackupDiffLineRsp>> diffCurrent(NetworkDeviceBackupRecordDiffCurrentReq request) {
        return new Result<List<NetworkDeviceBackupDiffLineRsp>>().ok(deviceBackupRecordService.diffCurrent(request));
    }

    @GetMapping("preview")
    @Operation(summary = "预览网络设备备份内容")
    @SaCheckPermission("ops:network-device-backup-record:preview")
    public Result<String> preview(NetworkDeviceBackupRecordPreviewReq request) {
        return new Result<String>().ok(deviceBackupRecordService.preview(request));
    }

    @GetMapping("download")
    @Operation(summary = "下载网络设备备份文件")
    @SaCheckPermission("ops:network-device-backup-record:download")
    public void download(NetworkDeviceBackupRecordDownloadReq request, HttpServletResponse response) {
        deviceBackupRecordService.download(request, response);
    }
}
