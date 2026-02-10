package net.leoch.modules.alert.service;

import cn.hutool.core.util.StrUtil;
import com.fasterxml.jackson.core.type.TypeReference;
import net.leoch.common.exception.ServiceException;
import net.leoch.common.utils.convert.JsonUtils;
import org.springframework.stereotype.Service;

import java.util.Map;

/**
 * 告警Webhook处理
 */
@Service
public class AlertWebhookService {

    private final IAlertRecordService alertRecordService;
    private final IAlertTriggerService alertTriggerService;

    public AlertWebhookService(IAlertRecordService alertRecordService, IAlertTriggerService alertTriggerService) {
        this.alertRecordService = alertRecordService;
        this.alertTriggerService = alertTriggerService;
    }

    public void handle(String severity, String rawJson) {
        if (StrUtil.isBlank(rawJson)) {
            throw new ServiceException("payload不能为空");
        }
        Map<String, Object> payload = JsonUtils.parseObject(rawJson, new TypeReference<Map<String, Object>>() {});
        String actualSeverity = severityFromPayload(payload, severity);
        alertRecordService.saveFromWebhook(payload, rawJson, actualSeverity);
        alertTriggerService.notifyFromWebhook(payload, rawJson, actualSeverity);
    }

    @SuppressWarnings("unchecked")
    private String severityFromPayload(Map<String, Object> payload, String fallback) {
        if (payload == null) {
            return fallback;
        }
        Object status = payload.get("status");
        if ("resolved".equalsIgnoreCase(String.valueOf(status))) {
            return "recover";
        }
        Object commonLabels = payload.get("commonLabels");
        if (commonLabels instanceof Map) {
            Object severity = ((Map<String, Object>) commonLabels).get("severity");
            if (severity != null) {
                return String.valueOf(severity);
            }
        }
        return fallback;
    }
}
