package net.leoch.modules.oss.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.AllArgsConstructor;
import net.leoch.common.constant.Constant;
import net.leoch.common.utils.JsonUtils;
import net.leoch.modules.oss.cloud.CloudStorageConfig;
import net.leoch.modules.oss.dao.SysOssConfigDao;
import net.leoch.modules.oss.entity.SysOssConfigEntity;
import net.leoch.modules.oss.service.SysOssConfigService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * 云存储配置
 *
 * @author Taohongqiang
 */
@Service
@AllArgsConstructor
public class SysOssConfigServiceImpl extends ServiceImpl<SysOssConfigDao, SysOssConfigEntity> implements SysOssConfigService {
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
