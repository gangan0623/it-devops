package net.leoch.modules.oss.controller;

import cn.dev33.satoken.annotation.SaCheckPermission;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import net.leoch.common.annotation.LogOperation;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.Result;
import net.leoch.common.cloud.CloudStorageConfig;
import net.leoch.modules.oss.vo.rsp.SysOssRsp;
import net.leoch.modules.oss.service.ISysOssConfigService;
import net.leoch.modules.oss.service.ISysOssService;
import net.leoch.modules.sys.vo.req.SysOssPageReq;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.util.Map;

/**
 * 文件上传
 *
 * @author Taohongqiang
 */
@RestController
@RequestMapping("sys/oss")
@Tag(name = "文件上传")
@RequiredArgsConstructor
public class SysOssController {
    private final ISysOssService sysOssService;
    private final ISysOssConfigService sysOssConfigService;

    @GetMapping("page")
    @Operation(summary = "分页")
    @SaCheckPermission("sys:oss:all")
    public Result<PageData<SysOssRsp>> page(SysOssPageReq request) {
        return new Result<PageData<SysOssRsp>>().ok(sysOssService.page(request));
    }

    @GetMapping("info")
    @Operation(summary = "云存储配置信息")
    @SaCheckPermission("sys:oss:all")
    public Result<CloudStorageConfig> info() {
        return new Result<CloudStorageConfig>().ok(sysOssConfigService.getConfig());
    }

    @PostMapping
    @Operation(summary = "保存云存储配置信息")
    @LogOperation("保存云存储配置信息")
    @SaCheckPermission("sys:oss:all")
    public Result<Object> saveConfig(@RequestBody CloudStorageConfig config) {
        sysOssConfigService.saveConfig(config);
        return new Result<>();
    }

    @PostMapping("upload")
    @Operation(summary = "上传文件")
    @SaCheckPermission("sys:oss:all")
    public Result<Map<String, Object>> upload(@RequestParam("file") MultipartFile file) throws Exception {
        return new Result<Map<String, Object>>().ok(sysOssService.upload(file));
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @SaCheckPermission("sys:oss:all")
    public Result<Object> delete(@RequestBody Long[] ids) {
        sysOssService.delete(ids);
        return new Result<>();
    }

}
