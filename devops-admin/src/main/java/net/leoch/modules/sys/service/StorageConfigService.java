package net.leoch.modules.sys.service;

import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONUtil;
import io.minio.MinioClient;
import io.minio.RemoveObjectArgs;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.base.Constant;
import net.leoch.common.exception.ServiceException;
import net.leoch.common.integration.storage.CloudStorageConfig;
import net.leoch.modules.sys.vo.req.SysParamsReq;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.net.URI;
import java.util.Objects;

/**
 * 存储配置（统一走sys_params）
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class StorageConfigService {
    private final ISysParamsService sysParamsService;

    public CloudStorageConfig getConfig() {
        CloudStorageConfig config = sysParamsService.getValueObject(Constant.MINIO_CONFIG_KEY, CloudStorageConfig.class);
        if (config == null) {
            config = new CloudStorageConfig();
        }
        config.setType(Constant.CloudService.MINIO.getValue());
        return config;
    }

    @Transactional(rollbackFor = Exception.class)
    public void saveMinioConfig(CloudStorageConfig config) {
        if (config == null) {
            throw new ServiceException("存储配置不能为空");
        }
        config.setType(Constant.CloudService.MINIO.getValue());
        validateMinioConfig(config);
        String json = JSONUtil.toJsonStr(config);
        int count = sysParamsService.updateValueByCode(Constant.MINIO_CONFIG_KEY, json);
        if (count > 0) {
            return;
        }
        SysParamsReq req = new SysParamsReq();
        req.setParamCode(Constant.MINIO_CONFIG_KEY);
        req.setParamValue(json);
        req.setRemark("MinIO配置");
        sysParamsService.save(req);
    }

    public void deleteMinioObjectByUrl(String url) {
        if (StrUtil.isBlank(url)) {
            throw new ServiceException("url不能为空");
        }
        CloudStorageConfig config = getConfig();
        validateMinioConfig(config);

        String objectKey = extractMinioObjectKey(url.trim(), config.getMinioBucketName());
        if (StrUtil.isBlank(objectKey)) {
            throw new ServiceException("无法从URL解析MinIO对象路径");
        }

        try {
            buildMinioClient(config).removeObject(
                RemoveObjectArgs.builder()
                    .bucket(config.getMinioBucketName())
                    .object(objectKey)
                    .build()
            );
            log.info("[存储配置] 已删除MinIO对象, object={}", objectKey);
        } catch (Exception e) {
            log.warn("[存储配置] 删除MinIO对象失败, object={}", objectKey, e);
            throw new ServiceException("删除MinIO对象失败");
        }
    }

    public String extractMinioObjectKey(String url, String bucket) {
        if (StrUtil.isBlank(url) || StrUtil.isBlank(bucket)) {
            return null;
        }
        try {
            URI uri = URI.create(url);
            String path = uri.getPath();
            if (StrUtil.isBlank(path)) {
                return null;
            }
            String prefix = "/" + bucket + "/";
            if (path.startsWith(prefix)) {
                return path.substring(prefix.length());
            }
            return null;
        } catch (Exception e) {
            log.warn("[存储配置] 解析MinIO URL失败, url={}", url, e);
            return null;
        }
    }

    public MinioClient buildMinioClient(CloudStorageConfig config) {
        return MinioClient.builder()
            .endpoint(config.getMinioEndPoint())
            .credentials(config.getMinioAccessKey(), config.getMinioSecretKey())
            .build();
    }

    public void validateMinioConfig(CloudStorageConfig config) {
        if (config == null || !Objects.equals(config.getType(), Constant.CloudService.MINIO.getValue())) {
            throw new ServiceException("当前仅支持MinIO配置");
        }
        if (StrUtil.hasBlank(config.getMinioEndPoint(), config.getMinioAccessKey(), config.getMinioSecretKey(), config.getMinioBucketName())) {
            throw new ServiceException("MinIO配置不完整");
        }
    }
}
