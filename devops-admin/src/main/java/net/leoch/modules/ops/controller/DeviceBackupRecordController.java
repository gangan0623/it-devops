package net.leoch.modules.ops.controller;

import cn.dev33.satoken.annotation.SaCheckPermission;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletResponse;
import net.leoch.common.annotation.LogOperation;
import net.leoch.common.constant.Constant;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.Result;
import net.leoch.modules.ops.dto.*;
import net.leoch.modules.ops.service.IDeviceBackupRecordService;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 设备备份信息表
 */
@RestController
@RequestMapping("ops/devicebackuprecord")
@Tag(name="设备备份信息表")
public class DeviceBackupRecordController {

    private final IDeviceBackupRecordService deviceBackupRecordService;

    public DeviceBackupRecordController(IDeviceBackupRecordService deviceBackupRecordService) {
        this.deviceBackupRecordService = deviceBackupRecordService;
    }

    @GetMapping("page")
    @Operation(summary = "分页")
    @Parameters({
        @Parameter(name = Constant.PAGE, description = "当前页码，从1开始", in = ParameterIn.QUERY, required = true, ref="int") ,
        @Parameter(name = Constant.LIMIT, description = "每页显示记录数", in = ParameterIn.QUERY,required = true, ref="int") ,
        @Parameter(name = Constant.ORDER_FIELD, description = "排序字段", in = ParameterIn.QUERY, ref="String") ,
        @Parameter(name = Constant.ORDER, description = "排序方式，可选值(asc、desc)", in = ParameterIn.QUERY, ref="String")
    })
    @SaCheckPermission("ops:devicebackuprecord:page")
    public Result<PageData<DeviceBackupRecordDTO>> page(@Parameter(hidden = true) DeviceBackupRecordPageRequest request){
        return new Result<PageData<DeviceBackupRecordDTO>>().ok(deviceBackupRecordService.page(request));
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @SaCheckPermission("ops:devicebackuprecord:info")
    public Result<DeviceBackupRecordDTO> get(@PathVariable Long id){
        return new Result<DeviceBackupRecordDTO>().ok(deviceBackupRecordService.get(DeviceBackupRecordIdRequest.of(id)));
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @SaCheckPermission("ops:devicebackuprecord:delete")
    public Result<Object> delete(@RequestBody Long[] ids){
        DeviceBackupRecordDeleteRequest request = new DeviceBackupRecordDeleteRequest();
        request.setIds(ids);
        deviceBackupRecordService.delete(request);
        return new Result<>();
    }

    @GetMapping("history")
    @Operation(summary = "历史列表")
    @SaCheckPermission("ops:devicebackuprecord:history")
    public Result<List<DeviceBackupHistoryDTO>> history(DeviceBackupRecordHistoryRequest request) {
        return new Result<List<DeviceBackupHistoryDTO>>().ok(deviceBackupRecordService.history(request));
    }

    @GetMapping("diff")
    @Operation(summary = "历史对比")
    @SaCheckPermission("ops:devicebackuprecord:diff")
    public Result<List<DeviceBackupDiffLineDTO>> diff(DeviceBackupRecordDiffRequest request) {
        return new Result<List<DeviceBackupDiffLineDTO>>().ok(deviceBackupRecordService.diff(request));
    }

    @GetMapping("diff-current")
    @Operation(summary = "历史对比当前")
    @SaCheckPermission("ops:devicebackuprecord:diff")
    public Result<List<DeviceBackupDiffLineDTO>> diffCurrent(DeviceBackupRecordDiffCurrentRequest request) {
        return new Result<List<DeviceBackupDiffLineDTO>>().ok(deviceBackupRecordService.diffCurrent(request));
    }

    @GetMapping("preview")
    @Operation(summary = "预览备份内容")
    @SaCheckPermission("ops:devicebackuprecord:preview")
    public Result<String> preview(DeviceBackupRecordPreviewRequest request) {
        return new Result<String>().ok(deviceBackupRecordService.preview(request));
    }

    @GetMapping("download")
    @Operation(summary = "下载备份文件")
    @SaCheckPermission("ops:devicebackuprecord:download")
    public void download(DeviceBackupRecordDownloadRequest request, HttpServletResponse response) {
        deviceBackupRecordService.download(request, response);
    }
}
