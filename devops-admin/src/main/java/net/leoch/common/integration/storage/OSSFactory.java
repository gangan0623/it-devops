

package net.leoch.common.integration.storage;

import net.leoch.common.exception.ServiceException;
import net.leoch.common.utils.context.SpringContextUtils;
import net.leoch.modules.sys.service.StorageConfigService;

/**
 * 文件上传Factory
 * @author Taohongqiang
 */
public final class OSSFactory {
    private static StorageConfigService storageConfigService;

    static {
        OSSFactory.storageConfigService = SpringContextUtils.getBean(StorageConfigService.class);
    }

    public static AbstractCloudStorageService build(){
        //获取云存储配置信息
        CloudStorageConfig config = storageConfigService.getConfig();
        if (config == null) {
            throw new ServiceException("云存储配置不存在");
        }

        return switch (config.getType()) {
            case 1 -> new QiniuCloudStorageService(config);     // QINIU
            case 2 -> new AliyunCloudStorageService(config);    // ALIYUN
            case 3 -> new QcloudCloudStorageService(config);    // QCLOUD
            case 4 -> new MinioCloudStorageService(config);     // MINIO
            default -> throw new ServiceException("不支持的存储类型: " + config.getType());
        };
    }

}
