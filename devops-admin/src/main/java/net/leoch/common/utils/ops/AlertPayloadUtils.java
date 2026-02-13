package net.leoch.common.utils.alert;

import cn.hutool.core.util.StrUtil;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public final class AlertPayloadUtils {
    private AlertPayloadUtils() {
    }

    public static List<Map<String, Object>> getAlerts(Map<String, Object> payload) {
        Object alerts = payload == null ? null : payload.get("alerts");
        if (!(alerts instanceof List)) {
            return new ArrayList<>();
        }
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> list = (List<Map<String, Object>>) alerts;
        return list;
    }

    public static Map<String, Object> buildContext(Map<String, Object> payload, Map<String, Object> alert, String severityFromPath) {
        Map<String, Object> context = new HashMap<>();
        if (payload != null) {
            context.put("receiver", payload.get("receiver"));
            context.put("status", payload.get("status"));
            context.put("externalURL", payload.get("externalURL"));
            context.put("groupKey", payload.get("groupKey"));
            context.put("commonLabels", payload.get("commonLabels"));
            context.put("commonAnnotations", payload.get("commonAnnotations"));
        }
        context.put("labels", alert == null ? null : alert.get("labels"));
        context.put("annotations", alert == null ? null : alert.get("annotations"));
        context.put("alert", alert);
        Map<String, Object> labels = toMap(alert == null ? null : alert.get("labels"));
        Map<String, Object> annotations = toMap(alert == null ? null : alert.get("annotations"));
        Map<String, Object> commonLabels = toMap(payload == null ? null : payload.get("commonLabels"));
        Map<String, Object> commonAnnotations = toMap(payload == null ? null : payload.get("commonAnnotations"));
        context.put("alertname", getLabelValue(labels, commonLabels, "alertname"));
        String severity = getLabelValue(labels, commonLabels, "severity", severityFromPath);
        context.put("severity", toSeverityZh(severity));
        context.put("instance", getLabelValue(labels, commonLabels, "instance", getLabelValue(labels, commonLabels, "service")));
        context.put("summary", getLabelValue(annotations, commonAnnotations, "summary"));
        context.put("description", getLabelValue(annotations, commonAnnotations, "description"));
        context.put("startsAt", alert == null ? null : alert.get("startsAt"));
        context.put("endsAt", alert == null ? null : alert.get("endsAt"));
        return context;
    }

    public static String toSeverityZh(String severity) {
        if (StrUtil.isBlank(severity)) {
            return severity;
        }
        String value = severity.trim().toLowerCase();
        return switch (value) {
            case "critical" -> "灾难";
            case "warning" -> "重要";
            case "info" -> "信息";
            case "recover", "resolved" -> "恢复";
            default -> severity;
        };
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> toMap(Object value) {
        return value instanceof Map ? (Map<String, Object>) value : new HashMap<>();
    }

    private static String getLabelValue(Map<String, Object> primary, Map<String, Object> fallback, String key) {
        String value = primary != null && primary.containsKey(key) ? String.valueOf(primary.get(key)) : null;
        if (StrUtil.isNotBlank(value)) {
            return value;
        }
        return fallback != null && fallback.containsKey(key) ? String.valueOf(fallback.get(key)) : null;
    }

    private static String getLabelValue(Map<String, Object> primary, Map<String, Object> fallback, String key, String extraFallback) {
        String value = getLabelValue(primary, fallback, key);
        if (StrUtil.isNotBlank(value)) {
            return value;
        }
        return extraFallback;
    }
}
