

package net.leoch.modules.oss.cloud;

import cn.hutool.core.util.StrUtil;
import io.minio.MinioClient;
import io.minio.PutObjectArgs;
import net.leoch.common.exception.ErrorCode;
import net.leoch.common.exception.ServiceException;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

/**
 * MinIO
 *
 * @author Taohongqiang
 */
public class MinioCloudStorageService extends AbstractCloudStorageService {
    private static final long MIN_PART_SIZE = 10 * 1024 * 1024L;

    public MinioCloudStorageService(CloudStorageConfig config) {
        this.config = config;
    }

    @Override
    public String upload(byte[] data, String path) {
        return upload(new ByteArrayInputStream(data), data.length, path);
    }

    @Override
    public String upload(InputStream inputStream, String path) {
        return upload(inputStream, -1, path);
    }

    @Override
    public String uploadSuffix(byte[] data, String suffix) {
        return upload(data, getPath(normalizePrefix(config.getMinioPath()), suffix));
    }

    @Override
    public String uploadSuffix(InputStream inputStream, String suffix) {
        return upload(inputStream, getPath(normalizePrefix(config.getMinioPath()), suffix));
    }

    private String upload(InputStream inputStream, long size, String path) {
        try {
            MinioClient client = MinioClient.builder()
                .endpoint(config.getMinioEndPoint())
                .credentials(config.getMinioAccessKey(), config.getMinioSecretKey())
                .build();

            client.putObject(
                PutObjectArgs.builder()
                    .bucket(config.getMinioBucketName())
                    .object(path)
                    .stream(inputStream, size, MIN_PART_SIZE)
                    .build()
            );
        } catch (Exception e) {
            throw new ServiceException(ErrorCode.OSS_UPLOAD_FILE_ERROR, e, "");
        }

        return buildUrl(path);
    }

    private String buildUrl(String path) {
        String domain = config.getMinioDomain();
        String normalizedPath = trimPrefix(path);
        String bucket = config.getMinioBucketName();
        if (StrUtil.isNotBlank(domain)) {
            return trimSuffix(domain) + "/" + bucket + "/" + normalizedPath;
        }
        return trimSuffix(config.getMinioEndPoint()) + "/" + bucket + "/" + normalizedPath;
    }

    private String trimSuffix(String value) {
        if (StrUtil.isBlank(value)) {
            return value;
        }
        if (value.endsWith("/")) {
            return value.substring(0, value.length() - 1);
        }
        return value;
    }

    private String trimPrefix(String value) {
        if (StrUtil.isBlank(value)) {
            return value;
        }
        if (value.startsWith("/")) {
            return value.substring(1);
        }
        return value;
    }

    private String normalizePrefix(String value) {
        if (StrUtil.isBlank(value)) {
            return value;
        }
        String normalized = value.trim();
        normalized = trimPrefix(normalized);
        if (normalized.endsWith("/")) {
            normalized = normalized.substring(0, normalized.length() - 1);
        }
        return normalized;
    }
}
