package net.leoch.modules.sys.controller;

import cn.dev33.satoken.annotation.SaCheckPermission;
import cn.hutool.core.util.StrUtil;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import net.leoch.common.annotation.LogOperation;
import net.leoch.common.data.result.Result;
import net.leoch.common.exception.ServiceException;
import net.leoch.common.integration.storage.CloudStorageConfig;
import net.leoch.framework.config.ops.ZabbixConfig;
import net.leoch.modules.ops.service.ZabbixClient;
import net.leoch.modules.ops.service.ZabbixConfigService;
import net.leoch.modules.ops.service.ZabbixNetworkHostSyncService;
import net.leoch.modules.ops.vo.rsp.ZabbixNetworkHostSyncRsp;
import net.leoch.modules.sys.service.AiConfigService;
import net.leoch.modules.sys.service.StorageConfigService;
import net.leoch.modules.sys.service.ZabbixNetworkDeviceMappingService;
import net.leoch.modules.sys.vo.req.StorageDeleteUrlReq;
import net.leoch.modules.sys.vo.req.SysAiConfigReq;
import net.leoch.modules.sys.vo.req.ZabbixNetworkDeviceMappingPreviewReq;
import net.leoch.modules.sys.vo.req.ZabbixNetworkDeviceMappingReq;
import net.leoch.modules.sys.vo.rsp.SysAiConfigRsp;
import net.leoch.modules.sys.vo.rsp.ZabbixNetworkDeviceMappingOptionsRsp;
import net.leoch.modules.sys.vo.rsp.ZabbixNetworkDeviceMappingPreviewRsp;
import net.leoch.modules.sys.vo.rsp.ZabbixNetworkDeviceMappingRsp;
import net.leoch.modules.sys.vo.rsp.ZabbixVersionRsp;
import org.springframework.web.bind.annotation.*;

/**
 * 参数配置中心
 */
@RestController
@RequestMapping("/sys/config-center")
@Tag(name = "参数配置")
@RequiredArgsConstructor
public class SysConfigCenterController {
    private final StorageConfigService storageConfigService;
    private final ZabbixConfigService zabbixConfigService;
    private final ZabbixClient zabbixClient;
    private final AiConfigService aiConfigService;
    private final ZabbixNetworkDeviceMappingService zabbixNetworkDeviceMappingService;
    private final ZabbixNetworkHostSyncService zabbixNetworkHostSyncService;

    @GetMapping("storage")
    @Operation(summary = "获取存储配置")
    @SaCheckPermission("sys:params:page")
    public Result<CloudStorageConfig> getStorageConfig() {
        return new Result<CloudStorageConfig>().ok(storageConfigService.getConfig());
    }

    @PutMapping("storage")
    @Operation(summary = "保存存储配置")
    @LogOperation("保存存储配置")
    @SaCheckPermission("sys:params:update")
    public Result<Object> saveStorageConfig(@RequestBody CloudStorageConfig config) {
        storageConfigService.saveMinioConfig(config);
        return new Result<>();
    }

    @PostMapping("storage/delete-by-url")
    @Operation(summary = "按URL删除MinIO对象")
    @LogOperation("按URL删除MinIO对象")
    @SaCheckPermission("sys:params:update")
    public Result<Object> deleteStorageObjectByUrl(@Valid @RequestBody StorageDeleteUrlReq req) {
        storageConfigService.deleteMinioObjectByUrl(req.getUrl());
        return new Result<>();
    }

    @GetMapping("zabbix")
    @Operation(summary = "获取Zabbix配置")
    @SaCheckPermission("sys:params:page")
    public Result<ZabbixConfig> getZabbixConfig() {
        ZabbixConfig config = zabbixConfigService.getConfig();
        if (config.getStatus() == null) {
            config.setStatus(1);
        }
        return new Result<ZabbixConfig>().ok(config);
    }

    @PostMapping("zabbix/test")
    @Operation(summary = "测试Zabbix连接")
    @SaCheckPermission("sys:params:update")
    public Result<Object> testZabbix(@RequestBody ZabbixConfig config) {
        validateZabbixConfig(config);
        try {
            zabbixClient.testConnection(config);
        } catch (Exception e) {
            throw new ServiceException(StrUtil.blankToDefault(e.getMessage(), "Zabbix连接测试失败"));
        }
        return new Result<>();
    }

    @PostMapping("zabbix/version")
    @Operation(summary = "检测Zabbix版本")
    @SaCheckPermission("sys:params:update")
    public Result<ZabbixVersionRsp> checkZabbixVersion(@RequestBody ZabbixConfig config) {
        validateZabbixConfig(config);
        try {
            zabbixClient.testConnection(config);
        } catch (Exception e) {
            throw new ServiceException(StrUtil.blankToDefault(e.getMessage(), "Zabbix连接测试失败"));
        }
        return new Result<ZabbixVersionRsp>().ok(buildZabbixVersionRsp(config));
    }

    @GetMapping("zabbix/version")
    @Operation(summary = "获取已配置Zabbix版本信息")
    @SaCheckPermission("sys:params:page")
    public Result<ZabbixVersionRsp> getZabbixVersion() {
        ZabbixConfig config = zabbixConfigService.getConfig();
        if (config == null || StrUtil.isBlank(config.getUrl())
                || StrUtil.isBlank(config.getUsername()) || StrUtil.isBlank(config.getPassword())) {
            return new Result<ZabbixVersionRsp>().ok(new ZabbixVersionRsp());
        }
        try {
            return new Result<ZabbixVersionRsp>().ok(buildZabbixVersionRsp(config));
        } catch (Exception e) {
            throw new ServiceException(StrUtil.blankToDefault(e.getMessage(), "Zabbix版本检测失败"));
        }
    }

    @PutMapping("zabbix")
    @Operation(summary = "保存Zabbix配置")
    @LogOperation("保存Zabbix配置")
    @SaCheckPermission("sys:params:update")
    public Result<Object> saveZabbix(@RequestBody ZabbixConfig config) {
        validateZabbixConfig(config);
        try {
            zabbixClient.testConnection(config);
        } catch (Exception e) {
            throw new ServiceException(StrUtil.blankToDefault(e.getMessage(), "Zabbix连接测试失败"));
        }
        ZabbixConfig oldConfig = zabbixConfigService.getConfig();
        if ((config.getTemplates() == null || config.getTemplates().isEmpty()) && oldConfig != null) {
            config.setTemplates(oldConfig.getTemplates());
        }
        zabbixConfigService.saveConfig(config);
        return new Result<>();
    }

    @GetMapping("zabbix/network-device-mapping")
    @Operation(summary = "获取Zabbix网络设备映射配置")
    @SaCheckPermission("sys:params:page")
    public Result<ZabbixNetworkDeviceMappingRsp> getZabbixNetworkDeviceMapping() {
        return new Result<ZabbixNetworkDeviceMappingRsp>().ok(zabbixNetworkDeviceMappingService.getConfig());
    }

    @PutMapping("zabbix/network-device-mapping")
    @Operation(summary = "保存Zabbix网络设备映射配置")
    @LogOperation("保存Zabbix网络设备映射配置")
    @SaCheckPermission("sys:params:update")
    public Result<Object> saveZabbixNetworkDeviceMapping(@RequestBody ZabbixNetworkDeviceMappingReq req) {
        zabbixNetworkDeviceMappingService.saveConfig(req);
        return new Result<>();
    }

    @GetMapping("zabbix/network-device-mapping/options")
    @Operation(summary = "获取Zabbix网络设备映射选项")
    @SaCheckPermission("sys:params:page")
    public Result<ZabbixNetworkDeviceMappingOptionsRsp> getZabbixNetworkDeviceMappingOptions() {
        return new Result<ZabbixNetworkDeviceMappingOptionsRsp>().ok(zabbixNetworkDeviceMappingService.getOptions());
    }

    @PostMapping("zabbix/network-device-mapping/preview")
    @Operation(summary = "预览Zabbix网络设备映射")
    @SaCheckPermission("sys:params:page")
    public Result<ZabbixNetworkDeviceMappingPreviewRsp> previewZabbixNetworkDeviceMapping(
            @RequestBody(required = false) ZabbixNetworkDeviceMappingPreviewReq req) {
        return new Result<ZabbixNetworkDeviceMappingPreviewRsp>().ok(zabbixNetworkDeviceMappingService.preview(req));
    }

    @PostMapping("zabbix/network-device-mapping/sync")
    @Operation(summary = "手动同步Zabbix网络设备数据")
    @LogOperation("手动同步Zabbix网络设备数据")
    @SaCheckPermission("sys:params:update")
    public Result<ZabbixNetworkHostSyncRsp> syncZabbixNetworkHosts() {
        return new Result<ZabbixNetworkHostSyncRsp>().ok(zabbixNetworkHostSyncService.sync());
    }

    @GetMapping("ai")
    @Operation(summary = "获取AI配置")
    @SaCheckPermission("sys:params:page")
    public Result<SysAiConfigRsp> getAiConfig() {
        return new Result<SysAiConfigRsp>().ok(aiConfigService.getConfig());
    }

    @PostMapping("ai/test")
    @Operation(summary = "测试AI连接")
    @SaCheckPermission("sys:params:update")
    public Result<Object> testAi(@Valid @RequestBody SysAiConfigReq req) {
        aiConfigService.testConnection(req);
        return new Result<>();
    }

    @PutMapping("ai")
    @Operation(summary = "保存AI配置")
    @LogOperation("保存AI配置")
    @SaCheckPermission("sys:params:update")
    public Result<Object> saveAi(@Valid @RequestBody SysAiConfigReq req) {
        aiConfigService.saveConfig(req);
        return new Result<>();
    }

    private void validateZabbixConfig(ZabbixConfig config) {
        if (config == null) {
            throw new ServiceException("Zabbix配置不能为空");
        }
        if (StrUtil.isBlank(config.getUrl())) {
            throw new ServiceException("Zabbix URL不能为空");
        }
        if (StrUtil.isBlank(config.getUsername())) {
            throw new ServiceException("用户名不能为空");
        }
        if (StrUtil.isBlank(config.getPassword())) {
            throw new ServiceException("密码不能为空");
        }
        if (config.getStatus() == null) {
            config.setStatus(1);
        }
        if (StrUtil.isBlank(config.getName())) {
            config.setName("Zabbix");
        }
    }

    private ZabbixVersionRsp buildZabbixVersionRsp(ZabbixConfig config) {
        ZabbixVersionRsp rsp = new ZabbixVersionRsp();
        String currentVersion = zabbixClient.getApiVersion(config);
        String latestVersion = zabbixClient.fetchLatestVersion();
        rsp.setCurrentVersion(currentVersion);
        rsp.setLatestVersion(latestVersion);
        if (StrUtil.isNotBlank(currentVersion) && StrUtil.isNotBlank(latestVersion)) {
            rsp.setUpdateAvailable(zabbixClient.compareVersions(currentVersion, latestVersion) < 0 ? 1 : 0);
        }
        rsp.setUpgradeUrl("https://www.zabbix.com/download");
        return rsp;
    }
}
