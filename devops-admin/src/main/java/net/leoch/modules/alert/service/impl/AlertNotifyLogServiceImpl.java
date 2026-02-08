package net.leoch.modules.alert.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import net.leoch.modules.alert.mapper.AlertNotifyLogMapper;
import net.leoch.modules.alert.entity.AlertNotifyLogEntity;
import net.leoch.modules.alert.service.IAlertNotifyLogService;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 告警发送日志
 *
 * @author Taohongqiang
 */
@Slf4j
@Service
public class AlertNotifyLogServiceImpl extends ServiceImpl<AlertNotifyLogMapper, AlertNotifyLogEntity> implements IAlertNotifyLogService {

    @Override
    public Map<Long, AlertNotifyLogEntity> loadLatestByRecordIds(List<Long> recordIds) {
        Map<Long, AlertNotifyLogEntity> result = new HashMap<>();
        if (recordIds == null || recordIds.isEmpty()) {
            return result;
        }
        List<AlertNotifyLogEntity> logs = this.list(
            new LambdaQueryWrapper<AlertNotifyLogEntity>()
                .in(AlertNotifyLogEntity::getRecordId, recordIds)
                .orderByDesc(AlertNotifyLogEntity::getSendTime)
        );
        for (AlertNotifyLogEntity logEntity : logs) {
            if (logEntity.getRecordId() == null || result.containsKey(logEntity.getRecordId())) {
                continue;
            }
            result.put(logEntity.getRecordId(), logEntity);
        }
        return result;
    }

    @Override
    public Map<String, AlertNotifyLogEntity> loadLatestByAlerts(List<String> alertNames, List<String> instances) {
        Map<String, AlertNotifyLogEntity> result = new HashMap<>();
        if (alertNames == null || alertNames.isEmpty() || instances == null || instances.isEmpty()) {
            return result;
        }
        List<AlertNotifyLogEntity> logs = this.list(
            new LambdaQueryWrapper<AlertNotifyLogEntity>()
                .in(AlertNotifyLogEntity::getAlertName, alertNames)
                .in(AlertNotifyLogEntity::getInstance, instances)
                .orderByDesc(AlertNotifyLogEntity::getSendTime)
                .last("limit 2000")
        );
        for (AlertNotifyLogEntity logEntity : logs) {
            String key = buildKey(logEntity.getAlertName(), logEntity.getInstance());
            if (StrUtil.isBlank(key) || result.containsKey(key)) {
                continue;
            }
            result.put(key, logEntity);
        }
        return result;
    }

    private String buildKey(String alertName, String instance) {
        if (StrUtil.isBlank(alertName) || StrUtil.isBlank(instance)) {
            return null;
        }
        return alertName.trim() + "|" + instance.trim();
    }
}
