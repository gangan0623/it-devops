

package net.leoch.modules.oss.controller;

import cn.dev33.satoken.annotation.SaCheckPermission;
import cn.hutool.core.io.file.FileNameUtil;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.AllArgsConstructor;
import net.leoch.common.annotation.LogOperation;
import net.leoch.common.constant.Constant;
import net.leoch.common.exception.ErrorCode;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.Result;
import net.leoch.common.validator.ValidatorUtils;
import net.leoch.common.validator.group.AliyunGroup;
import net.leoch.common.validator.group.MinioGroup;
import net.leoch.common.validator.group.QcloudGroup;
import net.leoch.common.validator.group.QiniuGroup;
import net.leoch.modules.oss.cloud.CloudStorageConfig;
import net.leoch.modules.oss.cloud.OSSFactory;
import net.leoch.modules.oss.entity.SysOssEntity;
import net.leoch.modules.oss.service.SysOssConfigService;
import net.leoch.modules.oss.service.SysOssService;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

/**
 * 文件上传
 *
 * @author Taohongqiang
 */
@RestController
@RequestMapping("sys/oss")
@Tag(name = "文件上传")
@AllArgsConstructor
public class SysOssController {
    private final SysOssService sysOssService;
    private final SysOssConfigService sysOssConfigService;

    @GetMapping("page")
    @Operation(summary = "分页")
    @SaCheckPermission("sys:oss:all")
    public Result<PageData<SysOssEntity>> page(@Parameter(hidden = true) @RequestParam Map<String, Object> params) {
        PageData<SysOssEntity> page = sysOssService.page(params);

        return new Result<PageData<SysOssEntity>>().ok(page);
    }

    @GetMapping("info")
    @Operation(summary = "云存储配置信息")
    @SaCheckPermission("sys:oss:all")
    public Result<CloudStorageConfig> info() {
        CloudStorageConfig config = sysOssConfigService.getConfig();

        return new Result<CloudStorageConfig>().ok(config);
    }

    @PostMapping
    @Operation(summary = "保存云存储配置信息")
    @LogOperation("保存云存储配置信息")
    @SaCheckPermission("sys:oss:all")
    public Result<Object> saveConfig(@RequestBody CloudStorageConfig config) {
        //校验类型
        ValidatorUtils.validateEntity(config);

        if (config.getType() == Constant.CloudService.QINIU.getValue()) {
            //校验七牛数据
            ValidatorUtils.validateEntity(config, QiniuGroup.class);
        } else if (config.getType() == Constant.CloudService.ALIYUN.getValue()) {
            //校验阿里云数据
            ValidatorUtils.validateEntity(config, AliyunGroup.class);
        } else if (config.getType() == Constant.CloudService.QCLOUD.getValue()) {
            //校验腾讯云数据
            ValidatorUtils.validateEntity(config, QcloudGroup.class);
        } else if (config.getType() == Constant.CloudService.MINIO.getValue()) {
            ValidatorUtils.validateEntity(config, MinioGroup.class);
        }

        sysOssConfigService.saveConfig(config);

        return new Result<>();
    }

    @PostMapping("upload")
    @Operation(summary = "上传文件")
    @SaCheckPermission("sys:oss:all")
    public Result<Map<String, Object>> upload(@RequestParam("file") MultipartFile file) throws Exception {
        if (file.isEmpty()) {
            return new Result<Map<String, Object>>().error(ErrorCode.UPLOAD_FILE_EMPTY);
        }

        //上传文件
        String suffix = FileNameUtil.getSuffix(file.getOriginalFilename());
        String url = OSSFactory.build().uploadSuffix(file.getBytes(), suffix);

        //保存文件信息
        SysOssEntity ossEntity = new SysOssEntity();
        ossEntity.setUrl(url);
        ossEntity.setCreateDate(new Date());
        sysOssService.insert(ossEntity);

        Map<String, Object> data = new HashMap<>(1);
        data.put("src", url);

        return new Result<Map<String, Object>>().ok(data);
    }

    @DeleteMapping
    @Operation(summary = "删除")
    @LogOperation("删除")
    @SaCheckPermission("sys:oss:all")
    public Result<Object> delete(@RequestBody Long[] ids) {
        sysOssService.deleteBatchIds(Arrays.asList(ids));

        return new Result<>();
    }

}
