package net.leoch.modules.alert.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import net.leoch.common.service.impl.CrudServiceImpl;
import net.leoch.modules.alert.dao.AlertRecordDao;
import net.leoch.modules.alert.dto.AlertRecordDTO;
import net.leoch.modules.alert.entity.AlertRecordEntity;
import net.leoch.modules.alert.service.AlertRecordService;
import net.leoch.modules.alert.service.AlertSseService;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * 告警记录
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Service
public class AlertRecordServiceImpl extends CrudServiceImpl<AlertRecordDao, AlertRecordEntity, AlertRecordDTO> implements AlertRecordService {

    private final AlertSseService alertSseService;

    public AlertRecordServiceImpl(AlertSseService alertSseService) {
        this.alertSseService = alertSseService;
    }

    @Override
    public QueryWrapper<AlertRecordEntity> getWrapper(Map<String, Object> params) {
        String alertName = (String) params.get("alertName");
        String severity = (String) params.get("severity");
        String status = (String) params.get("status");

        QueryWrapper<AlertRecordEntity> wrapper = new QueryWrapper<>();
        wrapper.like(StrUtil.isNotBlank(alertName), "alert_name", alertName);
        wrapper.eq(StrUtil.isNotBlank(severity), "severity", severity);
        wrapper.eq(StrUtil.isNotBlank(status), "status", status);
        wrapper.orderByDesc("starts_at");

        return wrapper;
    }

    @Override
    public void saveFromWebhook(Map<String, Object> payload, String rawJson, String severityFromPath) {
        if (payload == null) {
            return;
        }
        Object alertsObj = payload.get("alerts");
        if (!(alertsObj instanceof List)) {
            return;
        }
        String receiver = toStr(payload.get("receiver"));
        String status = toStr(payload.get("status"));
        Map<String, Object> commonLabels = toMap(payload.get("commonLabels"));
        Map<String, Object> commonAnnotations = toMap(payload.get("commonAnnotations"));
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> alerts = (List<Map<String, Object>>) alertsObj;
        boolean saved = false;
        for (Map<String, Object> alert : alerts) {
            Map<String, Object> labels = toMap(alert.get("labels"));
            Map<String, Object> annotations = toMap(alert.get("annotations"));
            AlertRecordEntity entity = new AlertRecordEntity();
            entity.setAlertName(getValue(labels, commonLabels, "alertname"));
            entity.setStatus(toStr(alert.get("status"), status));
            entity.setSeverity(resolveSeverity(payload, alert, severityFromPath));
            entity.setInstance(getValue(labels, commonLabels, "instance", getValue(labels, commonLabels, "service")));
            entity.setSummary(getValue(annotations, commonAnnotations, "summary"));
            entity.setDescription(getValue(annotations, commonAnnotations, "description"));
            entity.setStartsAt(parseDate(toStr(alert.get("startsAt"))));
            entity.setEndsAt(parseDate(toStr(alert.get("endsAt"))));
            entity.setReceiver(receiver);
            entity.setRawJson(rawJson);
            baseDao.insert(entity);
            saved = true;
        }
        if (saved) {
            alertSseService.publishRecentAlerts();
        }
    }

    private String resolveSeverity(Map<String, Object> payload, Map<String, Object> alert, String fallback) {
        String alertStatus = alert == null ? null : toStr(alert.get("status"));
        if ("resolved".equalsIgnoreCase(alertStatus)) {
            return "recover";
        }
        Map<String, Object> labels = toMap(alert == null ? null : alert.get("labels"));
        String severity = getValue(labels, toMap(payload.get("commonLabels")), "severity");
        if (StrUtil.isNotBlank(severity)) {
            return severity;
        }
        Object status = payload == null ? null : payload.get("status");
        if ("resolved".equalsIgnoreCase(String.valueOf(status))) {
            return "recover";
        }
        return fallback;
    }

    private static String toStr(Object value) {
        return value == null ? null : String.valueOf(value);
    }

    private static String toStr(Object value, String fallback) {
        String str = toStr(value);
        return StrUtil.isNotBlank(str) ? str : fallback;
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> toMap(Object value) {
        return value instanceof Map ? (Map<String, Object>) value : null;
    }

    private static String getValue(Map<String, Object> primary, Map<String, Object> fallback, String key) {
        String val = primary != null ? toStr(primary.get(key)) : null;
        if (StrUtil.isNotBlank(val)) {
            return val;
        }
        return fallback != null ? toStr(fallback.get(key)) : null;
    }

    private static String getValue(Map<String, Object> primary, Map<String, Object> fallback, String key, String extraFallback) {
        String val = getValue(primary, fallback, key);
        if (StrUtil.isNotBlank(val)) {
            return val;
        }
        return extraFallback;
    }

    private static Date parseDate(String value) {
        if (StrUtil.isBlank(value)) {
            return null;
        }
        try {
            return Date.from(Instant.parse(value));
        } catch (Exception ignore) {
            return null;
        }
    }
}
