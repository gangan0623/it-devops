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
import net.leoch.modules.ops.service.BackupAgentService;
import org.apache.shiro.authz.annotation.RequiresPermissions;
import org.springframework.web.bind.annotation.*;


/**
 * 备份节点表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@RestController
@RequestMapping("ops/backupagent")
@Tag(name="备份节点表")
public class BackupAgentController {
    @Resource
    private BackupAgentService backupAgentService;

    @GetMapping("page")
    @Operation(summary = "分页")
    @Parameters({
        @Parameter(name = Constant.PAGE, description = "当前页码，从1开始", in = ParameterIn.QUERY, required = true, ref="int") ,
        @Parameter(name = Constant.LIMIT, description = "每页显示记录数", in = ParameterIn.QUERY,required = true, ref="int") ,
        @Parameter(name = Constant.ORDER_FIELD, description = "排序字段", in = ParameterIn.QUERY, ref="String") ,
        @Parameter(name = Constant.ORDER, description = "排序方式，可选值(asc、desc)", in = ParameterIn.QUERY, ref="String")
    })
    @RequiresPermissions("ops:backupagent:page")
    public Result<PageData<BackupAgentDTO>> page(@Parameter(hidden = true) BackupAgentPageRequest request){
        return new Result<PageData<BackupAgentDTO>>().ok(backupAgentService.page(request));
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @RequiresPermissions("ops:backupagent:info")
    public Result<BackupAgentDTO> get(@PathVariable("id") Long id){
        return new Result<BackupAgentDTO>().ok(backupAgentService.get(BackupAgentIdRequest.of(id)));
    }

    @PostMapping
    @Operation(summary = "保存")
    @LogOperation("保存")
    @RequiresPermissions("ops:backupagent:save")
    public Result<Object> save(@RequestBody BackupAgentSaveRequest request){
        backupAgentService.save(request);
        return new Result<>();
    }

    @PutMapping
    @Operation(summary = "修改")
    @LogOperation("修改")
    @RequiresPermissions("ops:backupagent:update")
    public Result<Object> update(@RequestBody BackupAgentUpdateRequest request){
        backupAgentService.update(request);
        return new Result<>();
    }

    @PutMapping("status")
    @Operation(summary = "批量状态更新")
    @LogOperation("批量状态更新")
    @RequiresPermissions("ops:backupagent:update")
    public Result<Object> updateStatus(@RequestBody BackupAgentStatusUpdateRequest request){
        backupAgentService.updateStatus(request);
        return new Result<>();
    }

    @PostMapping("import")
    @Operation(summary = "导入")
    @LogOperation("导入")
    @RequiresPermissions("ops:backupagent:import")
    public Result<Object> importExcel(@ModelAttribute BackupAgentImportRequest request) throws Exception {
        backupAgentService.importExcel(request);
        return new Result<>();
    }

    @GetMapping("template")
    @Operation(summary = "导入模板")
    @LogOperation("导入模板")
    @RequiresPermissions("ops:backupagent:template")
    public void template(HttpServletResponse response) throws Exception {
        backupAgentService.template(response);
    }

    @GetMapping("online")
    @Operation(summary = "在线状态")
    @RequiresPermissions("ops:backupagent:page")
    public Result<Boolean> online(BackupAgentOnlineRequest request){
        return new Result<Boolean>().ok(backupAgentService.online(request));
    }

    @GetMapping("check")
    @Operation(summary = "唯一校验")
    @RequiresPermissions("ops:backupagent:page")
    public Result<Boolean> check(BackupAgentCheckRequest request){
        return new Result<Boolean>().ok(backupAgentService.check(request));
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @RequiresPermissions("ops:backupagent:delete")
    public Result<Object> delete(@RequestBody Long[] ids){
        BackupAgentDeleteRequest request = new BackupAgentDeleteRequest();
        request.setIds(ids);
        backupAgentService.delete(request);
        return new Result<>();
    }

    @GetMapping("export")
    @Operation(summary = "导出")
    @LogOperation("导出")
    @RequiresPermissions("ops:backupagent:export")
    public void export(@Parameter(hidden = true) @ModelAttribute BackupAgentPageRequest request, HttpServletResponse response) throws Exception {
        backupAgentService.export(request, response);
    }

}
