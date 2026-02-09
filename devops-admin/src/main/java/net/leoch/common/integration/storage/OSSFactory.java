

package net.leoch.common.integration.storage;

import net.leoch.common.core.base.Constant;
import net.leoch.common.exception.ServiceException;
import net.leoch.common.utils.SpringContextUtils;
import net.leoch.modules.oss.service.ISysOssConfigService;

/**
 * 文件上传Factory
 * @author Taohongqiang
 */
public final class OSSFactory {
    private static ISysOssConfigService sysOssConfigService;

    static {
        OSSFactory.sysOssConfigService = SpringContextUtils.getBean(ISysOssConfigService.class);
    }

    public static AbstractCloudStorageService build(){
        //获取云存储配置信息
        CloudStorageConfig config = sysOssConfigService.getConfig();

        if(config.getType() == Constant.CloudService.QINIU.getValue()){
            return new QiniuCloudStorageService(config);
        }else if(config.getType() == Constant.CloudService.ALIYUN.getValue()){
            return new AliyunCloudStorageService(config);
        }else if(config.getType() == Constant.CloudService.QCLOUD.getValue()){
            return new QcloudCloudStorageService(config);
        }else if(config.getType() == Constant.CloudService.MINIO.getValue()){
            return new MinioCloudStorageService(config);
        }

        throw new ServiceException("不支持的存储类型: " + config.getType());
    }

}
