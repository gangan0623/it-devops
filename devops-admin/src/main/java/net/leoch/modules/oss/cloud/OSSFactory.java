

package net.leoch.modules.oss.cloud;

import net.leoch.common.constant.Constant;
import net.leoch.common.utils.SpringContextUtils;
import net.leoch.modules.oss.service.SysOssConfigService;

/**
 * 文件上传Factory
 * @author Taohongqiang
 */
public final class OSSFactory {
    private static SysOssConfigService sysOssConfigService;

    static {
        OSSFactory.sysOssConfigService = SpringContextUtils.getBean(SysOssConfigService.class);
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

        return null;
    }

}
