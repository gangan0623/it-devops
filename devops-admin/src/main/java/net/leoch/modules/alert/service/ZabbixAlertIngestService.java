package net.leoch.modules.alert.service;

import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.modules.alert.entity.ZabbixAlertEventEntity;
import net.leoch.modules.alert.entity.ZabbixAlertEventHistoryEntity;
import net.leoch.modules.alert.mapper.ZabbixAlertEventHistoryMapper;
import net.leoch.modules.alert.mapper.ZabbixAlertEventMapper;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Zabbix webhook 告警入库（独立于 tb_alert_record）
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class ZabbixAlertIngestService {

    private static final Pattern DURATION_PATTERN = Pattern.compile("(?:(\\d+)d)?\\s*(?:(\\d+)h)?\\s*(?:(\\d+)m)?\\s*(?:(\\d+)s)?", Pattern.CASE_INSENSITIVE);

    private final ZabbixAlertEventMapper eventMapper;
    private final ZabbixAlertEventHistoryMapper historyMapper;

    @Transactional(rollbackFor = Exception.class)
    public void ingest(String payload, Long webhookLogId) {
        if (StrUtil.isBlank(payload) || !JSONUtil.isTypeJSON(payload)) {
            return;
        }
        JSONObject json = JSONUtil.parseObj(payload);

        String eventId = firstNotBlank(read(json, "event_id"), read(json, "eventid"), UUID.randomUUID().toString().replace("-", ""));
        String status = normalizeStatus(read(json, "status"), read(json, "trigger_value"));
        Date eventTime = parseEventTime(read(json, "event_time"));

        ZabbixAlertEventHistoryEntity history = new ZabbixAlertEventHistoryEntity();
        fillCommon(history, json, payload, webhookLogId, eventId, status, eventTime);

        if (isDuplicateHistory(eventId, status, eventTime)) {
            log.info("[ZabbixAlert] duplicate history ignored, eventId={}, status={}, eventTime={}", eventId, status, eventTime);
            return;
        }
        historyMapper.insert(history);

        ZabbixAlertEventEntity event = eventMapper.selectOne(new LambdaQueryWrapper<ZabbixAlertEventEntity>()
                .eq(ZabbixAlertEventEntity::getEventId, eventId)
                .last("limit 1"));
        if (event == null) {
            event = new ZabbixAlertEventEntity();
            event.setEventId(eventId);
            event.setCreateDate(new Date());
            event.setFirstEventTime(eventTime);
        }

        fillCommon(event, json, payload, webhookLogId, eventId, status, eventTime);
        event.setLastEventTime(eventTime);

        if ("resolved".equals(status)) {
            event.setResolvedTime(eventTime);
            if (event.getFirstEventTime() != null && eventTime != null) {
                long sec = Math.max(0, (eventTime.getTime() - event.getFirstEventTime().getTime()) / 1000);
                event.setDurationSec(sec);
            } else {
                event.setDurationSec(parseDurationSec(read(json, "event_duration")));
            }
        }
        event.setUpdateDate(new Date());

        if (event.getId() == null) {
            eventMapper.insert(event);
        } else {
            eventMapper.updateById(event);
        }
    }

    private boolean isDuplicateHistory(String eventId, String status, Date eventTime) {
        LambdaQueryWrapper<ZabbixAlertEventHistoryEntity> wrapper = new LambdaQueryWrapper<ZabbixAlertEventHistoryEntity>()
                .eq(ZabbixAlertEventHistoryEntity::getEventId, eventId)
                .eq(ZabbixAlertEventHistoryEntity::getStatus, status);
        if (eventTime == null) {
            wrapper.isNull(ZabbixAlertEventHistoryEntity::getEventTime);
        } else {
            wrapper.eq(ZabbixAlertEventHistoryEntity::getEventTime, eventTime);
        }
        Long count = historyMapper.selectCount(wrapper);
        return count != null && count > 0;
    }

    private void fillCommon(ZabbixAlertEventEntity target,
                            JSONObject json,
                            String payload,
                            Long webhookLogId,
                            String eventId,
                            String status,
                            Date eventTime) {
        target.setEventId(eventId);
        target.setTriggerId(read(json, "trigger_id"));
        target.setHostId(read(json, "host_id"));
        target.setHostHost(read(json, "host_host"));
        target.setHostname(read(json, "hostname"));
        target.setHostIp(read(json, "host_ip"));
        target.setHostGroup(read(json, "host_group"));
        target.setSeverityCode(read(json, "severity"));
        target.setSeverityName(resolveSeverityName(read(json, "severity")));
        target.setTriggerName(read(json, "trigger_name"));
        target.setTriggerKey(read(json, "trigger_key"));
        target.setTriggerValue(read(json, "trigger_value"));
        target.setItemId(read(json, "item_id"));
        target.setItemName(read(json, "item_name"));
        target.setItemValue(read(json, "item_value"));
        target.setStatus(status);
        target.setEventTime(eventTime);
        target.setEventDuration(read(json, "event_duration"));
        target.setDurationSec(parseDurationSec(read(json, "event_duration")));
        target.setWebhookLogId(webhookLogId);
        target.setRawJson(payload);
    }

    private void fillCommon(ZabbixAlertEventHistoryEntity target,
                            JSONObject json,
                            String payload,
                            Long webhookLogId,
                            String eventId,
                            String status,
                            Date eventTime) {
        target.setEventId(eventId);
        target.setTriggerId(read(json, "trigger_id"));
        target.setHostId(read(json, "host_id"));
        target.setHostHost(read(json, "host_host"));
        target.setHostname(read(json, "hostname"));
        target.setHostIp(read(json, "host_ip"));
        target.setHostGroup(read(json, "host_group"));
        target.setSeverityCode(read(json, "severity"));
        target.setSeverityName(resolveSeverityName(read(json, "severity")));
        target.setTriggerName(read(json, "trigger_name"));
        target.setTriggerKey(read(json, "trigger_key"));
        target.setTriggerValue(read(json, "trigger_value"));
        target.setItemId(read(json, "item_id"));
        target.setItemName(read(json, "item_name"));
        target.setItemValue(read(json, "item_value"));
        target.setStatus(status);
        target.setEventTime(eventTime);
        target.setEventDuration(read(json, "event_duration"));
        target.setDurationSec(parseDurationSec(read(json, "event_duration")));
        target.setWebhookLogId(webhookLogId);
        target.setPayloadJson(payload);
        target.setCreateDate(new Date());
    }

    private String read(JSONObject json, String key) {
        if (json == null || StrUtil.isBlank(key)) {
            return null;
        }
        Object value = json.get(key);
        if (value == null) {
            return null;
        }
        String text = String.valueOf(value).trim();
        return StrUtil.isBlank(text) ? null : text;
    }

    private String normalizeStatus(String status, String triggerValue) {
        if ("resolved".equalsIgnoreCase(StrUtil.nullToEmpty(status))) {
            return "resolved";
        }
        if ("0".equals(StrUtil.nullToEmpty(triggerValue))) {
            return "resolved";
        }
        return "open";
    }

    private String resolveSeverityName(String severityCode) {
        if (StrUtil.isBlank(severityCode)) {
            return null;
        }
        return switch (severityCode.trim()) {
            case "0" -> "not_classified";
            case "1" -> "info";
            case "2" -> "warning";
            case "3" -> "average";
            case "4" -> "high";
            case "5" -> "disaster";
            default -> severityCode;
        };
    }

    private Date parseEventTime(String value) {
        if (StrUtil.isBlank(value)) {
            return new Date();
        }
        String text = value.trim();
        for (String pattern : new String[]{"yyyy.MM.dd HH:mm:ss", "yyyy-MM-dd HH:mm:ss", "yyyy/MM/dd HH:mm:ss"}) {
            try {
                return new SimpleDateFormat(pattern, Locale.CHINA).parse(text);
            } catch (ParseException ignore) {
                // ignore
            }
        }
        return new Date();
    }

    private Long parseDurationSec(String value) {
        if (StrUtil.isBlank(value)) {
            return null;
        }
        Matcher m = DURATION_PATTERN.matcher(value.trim());
        if (!m.matches()) {
            return null;
        }
        long d = parseLong(m.group(1));
        long h = parseLong(m.group(2));
        long min = parseLong(m.group(3));
        long s = parseLong(m.group(4));
        long total = d * 86400 + h * 3600 + min * 60 + s;
        return total <= 0 ? null : total;
    }

    private long parseLong(String value) {
        if (StrUtil.isBlank(value)) {
            return 0;
        }
        try {
            return Long.parseLong(value.trim());
        } catch (Exception e) {
            return 0;
        }
    }

    private String firstNotBlank(String... values) {
        if (values == null) {
            return null;
        }
        for (String value : values) {
            if (StrUtil.isNotBlank(value)) {
                return value.trim();
            }
        }
        return null;
    }
}
