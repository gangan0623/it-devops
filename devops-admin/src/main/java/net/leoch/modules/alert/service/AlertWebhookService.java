package net.leoch.modules.alert.service;

import cn.hutool.core.lang.TypeReference;
import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONUtil;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.exception.ServiceException;
import org.springframework.stereotype.Service;

import java.util.Map;

/**
 * 告警Webhook处理
 */
@Slf4j
@Service
public class AlertWebhookService {

    private final IAlertRecordService alertRecordService;
    private final IAlertTriggerService alertTriggerService;

    public AlertWebhookService(IAlertRecordService alertRecordService, IAlertTriggerService alertTriggerService) {
        this.alertRecordService = alertRecordService;
        this.alertTriggerService = alertTriggerService;
    }

    public void handle(String severity, String rawJson) {
        log.info("[告警Webhook] 接收到告警, severity={}, payloadSize={}", severity, rawJson != null ? rawJson.length() : 0);
        if (StrUtil.isBlank(rawJson)) {
            log.error("[告警Webhook] payload为空");
            throw new ServiceException("payload不能为空");
        }
        try {
            Map<String, Object> payload = JSONUtil.toBean(rawJson, new TypeReference<Map<String, Object>>() {}, false);
            String actualSeverity = severityFromPayload(payload, severity);
            log.debug("[告警Webhook] 解析完成, actualSeverity={}, status={}", actualSeverity, payload.get("status"));
            alertRecordService.saveFromWebhook(payload, rawJson, actualSeverity);
            alertTriggerService.notifyFromWebhook(payload, rawJson, actualSeverity);
            log.info("[告警Webhook] 处理完成, actualSeverity={}", actualSeverity);
        } catch (Exception e) {
            log.error("[告警Webhook] 处理失败, severity={}", severity, e);
            throw e;
        }
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
