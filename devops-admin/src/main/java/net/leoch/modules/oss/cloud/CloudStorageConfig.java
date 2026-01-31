/**
 * Copyright (c) 2018 人人开源 All rights reserved.
 * <p>
 * https://www.renren.io
 * <p>
 * 版权所有，侵权必究！
 */

package net.leoch.modules.oss.cloud;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;
import net.leoch.common.validator.group.AliyunGroup;
import net.leoch.common.validator.group.MinioGroup;
import net.leoch.common.validator.group.QcloudGroup;
import net.leoch.common.validator.group.QiniuGroup;
import org.hibernate.validator.constraints.Range;
import org.hibernate.validator.constraints.URL;

import java.io.Serializable;

/**
 * 云存储配置信息
 *
 * @author Taohongqiang
 */
@Data
@Schema(title = "云存储配置信息")
public class CloudStorageConfig implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(title = "类型 1：七牛  2：阿里云  3：腾讯云 ")
    @Range(min = 1, max = 4, message = "{oss.type.range}")
    private Integer type;

    @Schema(title = "MinIO domain")
    private String minioDomain;

    @Schema(title = "MinIO backup path")
    private String minioPath;

    @Schema(title = "MinIO EndPoint")
    @NotBlank(message = "{minio.endPoint.require}", groups = MinioGroup.class)
    private String minioEndPoint;

    @Schema(title = "MinIO AccessKey")
    @NotBlank(message = "{minio.accesskey.require}", groups = MinioGroup.class)
    private String minioAccessKey;

    @Schema(title = "MinIO SecretKey")
    @NotBlank(message = "{minio.secretkey.require}", groups = MinioGroup.class)
    private String minioSecretKey;

    @Schema(title = "MinIO BucketName")
    @NotBlank(message = "{minio.bucketname.require}", groups = MinioGroup.class)
    private String minioBucketName;

    @Schema(title = "七牛绑定的域名")
    @NotBlank(message = "{qiniu.domain.require}", groups = QiniuGroup.class)
    @URL(message = "{qiniu.domain.url}", groups = QiniuGroup.class)
    private String qiniuDomain;

    @Schema(title = "七牛路径前缀")
    private String qiniuPrefix;

    @Schema(title = "七牛ACCESS_KEY")
    @NotBlank(message = "{qiniu.accesskey.require}", groups = QiniuGroup.class)
    private String qiniuAccessKey;

    @Schema(title = "七牛SECRET_KEY")
    @NotBlank(message = "{qiniu.secretkey.require}", groups = QiniuGroup.class)
    private String qiniuSecretKey;

    @Schema(title = "七牛存储空间名")
    @NotBlank(message = "{qiniu.bucketName.require}", groups = QiniuGroup.class)
    private String qiniuBucketName;

    @Schema(title = "阿里云绑定的域名")
    @NotBlank(message = "{aliyun.domain.require}", groups = AliyunGroup.class)
    @URL(message = "{aliyun.domain.url}", groups = AliyunGroup.class)
    private String aliyunDomain;

    @Schema(title = "阿里云路径前缀")
    private String aliyunPrefix;

    @Schema(title = "阿里云EndPoint")
    @NotBlank(message = "{aliyun.endPoint.require}", groups = AliyunGroup.class)
    private String aliyunEndPoint;

    @Schema(title = "阿里云AccessKeyId")
    @NotBlank(message = "{aliyun.accesskeyid.require}", groups = AliyunGroup.class)
    private String aliyunAccessKeyId;

    @Schema(title = "阿里云AccessKeySecret")
    @NotBlank(message = "{aliyun.accesskeysecret.require}", groups = AliyunGroup.class)
    private String aliyunAccessKeySecret;

    @Schema(title = "阿里云BucketName")
    @NotBlank(message = "{aliyun.bucketName.require}", groups = AliyunGroup.class)
    private String aliyunBucketName;

    @Schema(title = "腾讯云绑定的域名")
    @NotBlank(message = "{qcloud.domain.require}", groups = QcloudGroup.class)
    @URL(message = "{qcloud.domain.url}", groups = QcloudGroup.class)
    private String qcloudDomain;

    @Schema(title = "腾讯云路径前缀")
    private String qcloudPrefix;

    @Schema(title = "腾讯云AppId")
    @NotNull(message = "{qcloud.appid.require}", groups = QcloudGroup.class)
    private Integer qcloudAppId;

    @Schema(title = "腾讯云SecretId")
    @NotBlank(message = "{qcloud.secretId.require}", groups = QcloudGroup.class)
    private String qcloudSecretId;

    @Schema(title = "腾讯云SecretKey")
    @NotBlank(message = "{qcloud.secretkey.require}", groups = QcloudGroup.class)
    private String qcloudSecretKey;

    @Schema(title = "腾讯云BucketName")
    @NotBlank(message = "{qcloud.bucketName.require}", groups = QcloudGroup.class)
    private String qcloudBucketName;

    @Schema(title = "腾讯云COS所属地区")
    @NotBlank(message = "{qcloud.region.require}", groups = QcloudGroup.class)
    private String qcloudRegion;

}
