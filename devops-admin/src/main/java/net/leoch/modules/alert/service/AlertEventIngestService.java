package net.leoch.modules.alert.service;

import cn.hutool.core.util.StrUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.utils.common.ParseUtils;
import net.leoch.common.utils.common.TimeUtils;
import net.leoch.modules.alert.entity.AlertEventEntity;
import net.leoch.modules.alert.entity.AlertWebhookEventEntity;
import net.leoch.modules.alert.mapper.AlertEventMapper;
import net.leoch.modules.alert.mapper.AlertWebhookEventMapper;
import net.leoch.modules.alert.service.dto.AlertWebhookRecordResult;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * 告警Webhook事件持久化
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class AlertEventIngestService {

    private final AlertWebhookEventMapper alertWebhookEventMapper;
    private final AlertEventMapper alertEventMapper;

    @Transactional(rollbackFor = Exception.class)
    public Long persist(Map<String, Object> payload,
                        String rawJson,
                        List<AlertWebhookRecordResult> results) {
        if (payload == null || StrUtil.isBlank(rawJson)) {
            return null;
        }
        List<Map<String, Object>> alerts = extractAlerts(payload);
        AlertWebhookEventEntity webhookEvent = new AlertWebhookEventEntity();
        webhookEvent.setSource("alertmanager");
        webhookEvent.setReceiver(asText(payload.get("receiver")));
        webhookEvent.setStatus(asText(payload.get("status")));
        webhookEvent.setPayloadJson(rawJson);
        webhookEvent.setCreateDate(new Date());
        alertWebhookEventMapper.insert(webhookEvent);

        if (results == null || results.isEmpty()) {
            return webhookEvent.getId();
        }

        Date eventTime = webhookEvent.getCreateDate();
        for (AlertWebhookRecordResult result : results) {
            if (result == null || result.getAlert() == null) {
                continue;
            }
            Map<String, Object> alert = result.getAlert();
            Map<String, Object> labels = asMap(alert.get("labels"));
            AlertEventEntity event = new AlertEventEntity();
            event.setWebhookEventId(webhookEvent.getId());
            event.setFingerprint(asText(alert.get("fingerprint")));
            event.setAlertName(firstNonBlank(asText(labels.get("alertname")),
                    result.getRecord() == null ? null : result.getRecord().getAlertName()));
            event.setInstance(firstNonBlank(asText(labels.get("instance")), asText(labels.get("service")),
                    result.getRecord() == null ? null : result.getRecord().getInstance()));
            event.setStatus(firstNonBlank(asText(alert.get("status")),
                    result.getRecord() == null ? null : result.getRecord().getStatus(),
                    webhookEvent.getStatus()));
            event.setSeverity(result.getRecord() == null ? null : result.getRecord().getSeverity());
            event.setSummary(result.getRecord() == null ? null : result.getRecord().getSummary());
            event.setDescription(result.getRecord() == null ? null : result.getRecord().getDescription());
            event.setStartsAt(TimeUtils.parseDate(ParseUtils.toStr(alert.get("startsAt"))));
            event.setEndsAt(TimeUtils.parseDate(ParseUtils.toStr(alert.get("endsAt"))));
            event.setReceiver(webhookEvent.getReceiver());
            event.setAlertGroup(firstNonBlank(asText(labels.get("alertgroup")),
                    result.getRecord() == null ? null : result.getRecord().getAlertGroup()));
            event.setEventTime(eventTime);
            event.setRecordId(result.getRecord() == null ? null : result.getRecord().getId());
            event.setCreateDate(eventTime);
            alertEventMapper.insert(event);
        }
        log.info("[AlertEventIngest] webhook已落库, webhookEventId={}, events={}", webhookEvent.getId(), results.size());
        return webhookEvent.getId();
    }

    @SuppressWarnings("unchecked")
    private List<Map<String, Object>> extractAlerts(Map<String, Object> payload) {
        Object alerts = payload.get("alerts");
        return alerts instanceof List ? (List<Map<String, Object>>) alerts : java.util.Collections.emptyList();
    }

    private String asText(Object value) {
        return value == null ? null : String.valueOf(value);
    }

    @SuppressWarnings("unchecked")
    private Map<String, Object> asMap(Object value) {
        return value instanceof Map ? (Map<String, Object>) value : null;
    }

    private String firstNonBlank(String... values) {
        if (values == null) {
            return null;
        }
        for (String value : values) {
            if (StrUtil.isNotBlank(value)) {
                return value;
            }
        }
        return null;
    }
}
