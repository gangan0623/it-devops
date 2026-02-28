package net.leoch.modules.ops.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import io.minio.MinioClient;
import io.minio.RemoveObjectArgs;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.base.Constant;
import net.leoch.common.integration.storage.CloudStorageConfig;
import net.leoch.modules.alert.entity.AlertRecordActionEntity;
import net.leoch.modules.alert.entity.AlertRecordEntity;
import net.leoch.modules.alert.mapper.AlertRecordActionMapper;
import net.leoch.modules.alert.mapper.AlertRecordMapper;
import net.leoch.modules.ops.entity.DeviceBackupHistoryEntity;
import net.leoch.modules.ops.entity.DeviceBackupRecordEntity;
import net.leoch.modules.ops.mapper.DeviceBackupHistoryMapper;
import net.leoch.modules.ops.mapper.DeviceBackupRecordMapper;
import net.leoch.modules.ops.vo.rsp.OpsDeleteCascadeRsp;
import net.leoch.modules.sys.service.StorageConfigService;
import org.springframework.stereotype.Service;

import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;

@Slf4j
@Service
@RequiredArgsConstructor
public class OpsDeleteCascadeService {

    private final AlertRecordMapper alertRecordMapper;
    private final AlertRecordActionMapper alertRecordActionMapper;
    private final DeviceBackupRecordMapper deviceBackupRecordMapper;
    private final DeviceBackupHistoryMapper deviceBackupHistoryMapper;
    private final StorageConfigService storageConfigService;

    public OpsDeleteCascadeRsp deleteAlertRecordsByInstances(Collection<String> instances) {
        OpsDeleteCascadeRsp rsp = new OpsDeleteCascadeRsp();
        List<String> validInstances = normalizeStrings(instances);
        if (validInstances.isEmpty()) {
            return rsp;
        }

        List<AlertRecordEntity> records = alertRecordMapper.selectList(
                new LambdaQueryWrapper<AlertRecordEntity>()
                        .select(AlertRecordEntity::getId)
                        .in(AlertRecordEntity::getInstance, validInstances)
        );
        if (records.isEmpty()) {
            return rsp;
        }

        List<Long> recordIds = records.stream().map(AlertRecordEntity::getId).filter(Objects::nonNull).toList();
        int actionDeleted = 0;
        if (!recordIds.isEmpty()) {
            actionDeleted = alertRecordActionMapper.delete(new LambdaQueryWrapper<AlertRecordActionEntity>()
                    .in(AlertRecordActionEntity::getRecordId, recordIds));
        }
        int deleted = alertRecordMapper.delete(new LambdaQueryWrapper<AlertRecordEntity>()
                .in(AlertRecordEntity::getInstance, validInstances));
        rsp.setDeletedAlertRecords(deleted);
        rsp.setDeletedAlertActions(actionDeleted);
        log.info("[删除联动] 已删除告警记录, instances={}, records={}", validInstances.size(), deleted);
        return rsp;
    }

    public OpsDeleteCascadeRsp deleteDeviceBackupRecordsAndFilesByIps(Collection<String> ips) {
        OpsDeleteCascadeRsp rsp = new OpsDeleteCascadeRsp();
        List<String> validIps = normalizeStrings(ips);
        if (validIps.isEmpty()) {
            return rsp;
        }

        List<DeviceBackupHistoryEntity> histories = deviceBackupHistoryMapper.selectList(
                new LambdaQueryWrapper<DeviceBackupHistoryEntity>()
                        .select(DeviceBackupHistoryEntity::getId, DeviceBackupHistoryEntity::getIp, DeviceBackupHistoryEntity::getUrl)
                        .in(DeviceBackupHistoryEntity::getIp, validIps)
        );
        List<String> urls = histories.stream()
                .map(DeviceBackupHistoryEntity::getUrl)
                .filter(StrUtil::isNotBlank)
                .toList();

        if (!urls.isEmpty()) {
            rsp.setDeletedMinioTxtFiles(deleteMinioTxtFiles(urls));
        }

        int historyDeleted = deviceBackupHistoryMapper.delete(new LambdaQueryWrapper<DeviceBackupHistoryEntity>()
                .in(DeviceBackupHistoryEntity::getIp, validIps));
        int recordDeleted = deviceBackupRecordMapper.delete(new LambdaQueryWrapper<DeviceBackupRecordEntity>()
                .in(DeviceBackupRecordEntity::getIp, validIps));
        rsp.setDeletedBackupHistories(historyDeleted);
        rsp.setDeletedBackupRecords(recordDeleted);
        log.info("[删除联动] 已删除备份记录, ips={}, history={}, record={}", validIps.size(), historyDeleted, recordDeleted);
        return rsp;
    }

    private int deleteMinioTxtFiles(List<String> urls) {
        CloudStorageConfig config = storageConfigService.getConfig();
        if (config == null || !Objects.equals(config.getType(), Constant.CloudService.MINIO.getValue())) {
            log.info("[删除联动] 当前非MinIO存储，跳过对象删除");
            return 0;
        }
        if (StrUtil.isBlank(config.getMinioEndPoint())
                || StrUtil.isBlank(config.getMinioAccessKey())
                || StrUtil.isBlank(config.getMinioSecretKey())
                || StrUtil.isBlank(config.getMinioBucketName())) {
            log.warn("[删除联动] MinIO配置不完整，跳过对象删除");
            return 0;
        }

        MinioClient client = MinioClient.builder()
                .endpoint(config.getMinioEndPoint())
                .credentials(config.getMinioAccessKey(), config.getMinioSecretKey())
                .build();
        String bucket = config.getMinioBucketName();
        Set<String> objectKeys = urls.stream()
                .map(url -> extractMinioObjectKey(url, bucket))
                .filter(StrUtil::isNotBlank)
                .filter(this::isTxtObject)
                .collect(Collectors.toCollection(LinkedHashSet::new));

        int deletedCount = 0;
        for (String objectKey : objectKeys) {
            try {
                client.removeObject(RemoveObjectArgs.builder().bucket(bucket).object(objectKey).build());
                log.info("[删除联动] 已删除MinIO对象, bucket={}, object={}", bucket, objectKey);
                deletedCount++;
            } catch (Exception e) {
                log.warn("[删除联动] 删除MinIO对象失败, bucket={}, object={}", bucket, objectKey, e);
            }
        }
        return deletedCount;
    }

    private String extractMinioObjectKey(String url, String bucket) {
        if (StrUtil.isBlank(url) || StrUtil.isBlank(bucket)) {
            return null;
        }
        try {
            URI uri = URI.create(url.trim());
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
            log.warn("[删除联动] 解析MinIO URL失败, url={}", url, e);
            return null;
        }
    }

    private boolean isTxtObject(String objectKey) {
        if (StrUtil.isBlank(objectKey)) {
            return false;
        }
        return objectKey.toLowerCase(Locale.ROOT).endsWith(".txt");
    }

    private List<String> normalizeStrings(Collection<String> values) {
        if (values == null || values.isEmpty()) {
            return new ArrayList<>();
        }
        LinkedHashSet<String> set = new LinkedHashSet<>();
        for (String value : values) {
            if (StrUtil.isNotBlank(value)) {
                set.add(value.trim());
            }
        }
        return new ArrayList<>(set);
    }
}
