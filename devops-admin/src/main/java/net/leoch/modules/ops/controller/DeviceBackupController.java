package net.leoch.modules.ops.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.Resource;
import jakarta.servlet.http.HttpServletResponse;
import net.leoch.common.annotation.LogOperation;
import net.leoch.common.constant.Constant;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.Result;
import net.leoch.modules.ops.dto.*;
import net.leoch.modules.ops.service.DeviceBackupService;
import org.apache.shiro.authz.annotation.RequiresPermissions;
import org.springframework.web.bind.annotation.*;


/**
 * 设备备份表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@RestController
@RequestMapping("ops/devicebackup")
@Tag(name="设备备份表")
public class DeviceBackupController {

    @Resource
    private DeviceBackupService deviceBackupService;

    @GetMapping("page")
    @Operation(summary = "分页")
    @Parameters({
        @Parameter(name = Constant.PAGE, description = "当前页码，从1开始", in = ParameterIn.QUERY, required = true, ref="int") ,
        @Parameter(name = Constant.LIMIT, description = "每页显示记录数", in = ParameterIn.QUERY,required = true, ref="int") ,
        @Parameter(name = Constant.ORDER_FIELD, description = "排序字段", in = ParameterIn.QUERY, ref="String") ,
        @Parameter(name = Constant.ORDER, description = "排序方式，可选值(asc、desc)", in = ParameterIn.QUERY, ref="String")
    })
    @RequiresPermissions("ops:devicebackup:page")
    public Result<PageData<DeviceBackupDTO>> page(@Parameter(hidden = true) DeviceBackupPageRequest request){
        return new Result<PageData<DeviceBackupDTO>>().ok(deviceBackupService.page(request));
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @RequiresPermissions("ops:devicebackup:info")
    public Result<DeviceBackupDTO> get(@PathVariable Long id){
        return new Result<DeviceBackupDTO>().ok(deviceBackupService.get(DeviceBackupIdRequest.of(id)));
    }

    @PostMapping
    @Operation(summary = "保存")
    @LogOperation("保存")
    @RequiresPermissions("ops:devicebackup:save")
    public Result<Object> save(@RequestBody DeviceBackupSaveRequest request){
        deviceBackupService.save(request);
        return new Result<>();
    }

    @PutMapping
    @Operation(summary = "修改")
    @LogOperation("修改")
    @RequiresPermissions("ops:devicebackup:update")
    public Result<Object> update(@RequestBody DeviceBackupUpdateRequest request){
        deviceBackupService.update(request);
        return new Result<>();
    }

    @PutMapping("status")
    @Operation(summary = "批量状态更新")
    @LogOperation("批量状态更新")
    @RequiresPermissions("ops:devicebackup:update")
    public Result<Object> updateStatus(@RequestBody DeviceBackupStatusUpdateRequest request){
        deviceBackupService.updateStatus(request);
        return new Result<>();
    }

    @PostMapping("import")
    @Operation(summary = "导入")
    @LogOperation("导入")
    @RequiresPermissions("ops:devicebackup:import")
    public Result<Object> importExcel(@ModelAttribute DeviceBackupImportRequest request) throws Exception {
        deviceBackupService.importExcel(request);
        return new Result<>();
    }

    @GetMapping("template")
    @Operation(summary = "导入模板")
    @LogOperation("导入模板")
    @RequiresPermissions("ops:devicebackup:template")
    public void template(HttpServletResponse response) throws Exception {
        deviceBackupService.template(response);
    }

    @GetMapping("online")
    @Operation(summary = "在线状态")
    @RequiresPermissions("ops:devicebackup:page")
    public Result<Boolean> online(DeviceBackupOnlineRequest request){
        return new Result<Boolean>().ok(deviceBackupService.online(request));
    }

    @GetMapping("check")
    @Operation(summary = "唯一校验")
    @RequiresPermissions("ops:devicebackup:page")
    public Result<Boolean> check(DeviceBackupCheckRequest request){
        return new Result<Boolean>().ok(deviceBackupService.check(request));
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @RequiresPermissions("ops:devicebackup:delete")
    public Result<Object> delete(@RequestBody Long[] ids){
        DeviceBackupDeleteRequest request = new DeviceBackupDeleteRequest();
        request.setIds(ids);
        deviceBackupService.delete(request);
        return new Result<>();
    }

    @GetMapping("export")
    @Operation(summary = "导出")
    @LogOperation("导出")
    @RequiresPermissions("ops:devicebackup:export")
    public void export(@Parameter(hidden = true) @ModelAttribute DeviceBackupPageRequest request, HttpServletResponse response) throws Exception {
        deviceBackupService.export(request, response);
    }

}
