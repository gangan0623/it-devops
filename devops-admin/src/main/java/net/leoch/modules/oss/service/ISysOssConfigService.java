package net.leoch.modules.oss.service;

import net.leoch.common.integration.storage.CloudStorageConfig;

/**
 * 云存储配置
 *
 * @author Taohongqiang
 */
public interface ISysOssConfigService {
    CloudStorageConfig getConfig();

    void saveConfig(CloudStorageConfig config);
}
