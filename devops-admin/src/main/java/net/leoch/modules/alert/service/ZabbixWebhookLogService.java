package net.leoch.modules.alert.service;

import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.modules.alert.entity.ZabbixWebhookLogEntity;
import net.leoch.modules.alert.mapper.ZabbixWebhookLogMapper;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.Enumeration;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.UUID;

/**
 * Zabbix webhook 日志接收
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class ZabbixWebhookLogService {

    private final ZabbixWebhookLogMapper zabbixWebhookLogMapper;

    public Long receiveAndStore(HttpServletRequest request, String payload) {
        ZabbixWebhookLogEntity entity = new ZabbixWebhookLogEntity();
        Date now = new Date();
        entity.setSource("zabbix");
        entity.setRequestId(UUID.randomUUID().toString().replace("-", ""));
        entity.setRequestMethod(request == null ? null : request.getMethod());
        entity.setRequestUri(request == null ? null : request.getRequestURI());
        entity.setQueryString(request == null ? null : request.getQueryString());
        entity.setContentType(request == null ? null : request.getContentType());
        entity.setRemoteIp(resolveClientIp(request));
        entity.setUserAgent(request == null ? null : request.getHeader("User-Agent"));
        entity.setHeadersJson(toHeadersJson(request));
        entity.setPayloadRaw(payload);
        entity.setPayloadSize(payload == null ? 0 : payload.length());
        entity.setProcessStatus(1);
        entity.setProcessMessage("stored");
        entity.setReceivedAt(now);
        entity.setCreateDate(now);

        fillExtractedFields(entity, payload);

        zabbixWebhookLogMapper.insert(entity);
        log.info("[ZabbixWebhook] stored, id={}, uri={}, payloadSize={}",
                entity.getId(), entity.getRequestUri(), entity.getPayloadSize());
        return entity.getId();
    }

    private void fillExtractedFields(ZabbixWebhookLogEntity entity, String payload) {
        if (StrUtil.isBlank(payload) || !JSONUtil.isTypeJSON(payload)) {
            return;
        }
        try {
            JSONObject json = JSONUtil.parseObj(payload);

            // Zabbix 媒介脚本常用字段（保底提取，失败不影响主流程）
            entity.setEventId(readAsString(json, "event_id", "eventid", "eventId"));
            entity.setTriggerId(readAsString(json, "trigger_id", "triggerid", "triggerId"));
            entity.setHost(readAsString(json, "host", "hostname", "host_name"));
            entity.setSeverity(readAsString(json, "severity", "level", "priority"));
            entity.setStatus(readAsString(json, "status", "state"));
        } catch (Exception e) {
            entity.setProcessStatus(2);
            entity.setProcessMessage("json_parse_failed");
            log.warn("[ZabbixWebhook] payload parse failed", e);
        }
    }

    private String readAsString(JSONObject json, String... keys) {
        for (String key : keys) {
            Object value = json.get(key);
            if (value != null && StrUtil.isNotBlank(String.valueOf(value))) {
                return String.valueOf(value);
            }
        }
        return null;
    }

    private String toHeadersJson(HttpServletRequest request) {
        if (request == null) {
            return "{}";
        }
        Map<String, String> map = new LinkedHashMap<>();
        Enumeration<String> names = request.getHeaderNames();
        if (names == null) {
            return "{}";
        }
        while (names.hasMoreElements()) {
            String name = names.nextElement();
            map.put(name, request.getHeader(name));
        }
        return JSONUtil.toJsonStr(map);
    }

    private String resolveClientIp(HttpServletRequest request) {
        if (request == null) {
            return null;
        }
        String xff = request.getHeader("X-Forwarded-For");
        if (StrUtil.isNotBlank(xff)) {
            return xff.split(",")[0].trim();
        }
        String realIp = request.getHeader("X-Real-IP");
        if (StrUtil.isNotBlank(realIp)) {
            return realIp.trim();
        }
        return request.getRemoteAddr();
    }
}

