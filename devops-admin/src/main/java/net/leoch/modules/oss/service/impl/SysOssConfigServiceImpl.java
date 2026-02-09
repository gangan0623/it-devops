package net.leoch.modules.oss.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import net.leoch.common.core.base.Constant;
import net.leoch.common.utils.JsonUtils;
import net.leoch.common.data.validator.ValidatorUtils;
import net.leoch.common.data.validator.group.AliyunGroup;
import net.leoch.common.data.validator.group.MinioGroup;
import net.leoch.common.data.validator.group.QcloudGroup;
import net.leoch.common.data.validator.group.QiniuGroup;
import net.leoch.common.integration.storage.CloudStorageConfig;
import net.leoch.modules.oss.mapper.SysOssConfigMapper;
import net.leoch.modules.oss.entity.SysOssConfigEntity;
import net.leoch.modules.oss.service.ISysOssConfigService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * 云存储配置
 *
 * @author Taohongqiang
 */
@Service
@RequiredArgsConstructor
public class SysOssConfigServiceImpl extends ServiceImpl<SysOssConfigMapper, SysOssConfigEntity> implements ISysOssConfigService {
    private static final Long CONFIG_ID = 1L;

    @Override
    public CloudStorageConfig getConfig() {
        SysOssConfigEntity entity = this.getById(CONFIG_ID);
        if (entity == null || StrUtil.isBlank(entity.getConfigJson())) {
            CloudStorageConfig config = new CloudStorageConfig();
            if (config.getType() == null) {
                config.setType(Constant.CloudService.MINIO.getValue());
            }
            return config;
        }
        CloudStorageConfig config = JsonUtils.parseObject(entity.getConfigJson(), CloudStorageConfig.class);
        if (config.getType() == null) {
            config.setType(Constant.CloudService.MINIO.getValue());
        }
        return config;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveConfig(CloudStorageConfig config) {
        ValidatorUtils.validateEntity(config);
        if (config.getType() == Constant.CloudService.QINIU.getValue()) {
            ValidatorUtils.validateEntity(config, QiniuGroup.class);
        } else if (config.getType() == Constant.CloudService.ALIYUN.getValue()) {
            ValidatorUtils.validateEntity(config, AliyunGroup.class);
        } else if (config.getType() == Constant.CloudService.QCLOUD.getValue()) {
            ValidatorUtils.validateEntity(config, QcloudGroup.class);
        } else if (config.getType() == Constant.CloudService.MINIO.getValue()) {
            ValidatorUtils.validateEntity(config, MinioGroup.class);
        }

        SysOssConfigEntity entity = new SysOssConfigEntity();
        entity.setId(CONFIG_ID);
        entity.setConfigJson(JsonUtils.toJsonString(config));

        SysOssConfigEntity existing = this.getById(CONFIG_ID);
        if (existing == null) {
            this.save(entity);
        } else {
            this.updateById(entity);
        }
    }
}
