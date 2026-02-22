package net.leoch.modules.alert.service;

import cn.hutool.core.lang.TypeReference;
import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.integration.security.SecurityUser;
import net.leoch.framework.config.ops.HttpTimeoutConfig;
import net.leoch.modules.alert.entity.AlertRecordEntity;
import net.leoch.modules.ops.entity.MonitorComponentEntity;
import net.leoch.modules.ops.mapper.MonitorComponentMapper;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Alertmanager操作
 */
@Slf4j
@Service
public class AlertManagerService {

    private static final String TYPE_ALERTMANAGER = "alertmanager";

    @Value("${alert.manager.url:http://192.168.17.121:9093}")
    private String defaultAlertmanagerUrl;

    private final MonitorComponentMapper monitorComponentMapper;
    private final HttpTimeoutConfig httpTimeoutConfig;

    public AlertManagerService(MonitorComponentMapper monitorComponentMapper, HttpTimeoutConfig httpTimeoutConfig) {
        this.monitorComponentMapper = monitorComponentMapper;
        this.httpTimeoutConfig = httpTimeoutConfig;
    }

    public String createSilence(AlertRecordEntity record, int days, String message) {
        log.info("[告警管理] 创建静默, alertName={}, instance={}, days={}",
                 record != null ? record.getAlertName() : null,
                 record != null ? record.getInstance() : null,
                 days);
        if (record == null || days <= 0) {
            log.warn("[告警管理] 创建静默参数无效, record={}, days={}", record, days);
            return null;
        }
        String baseUrl = resolveAlertmanagerBaseUrl();
        if (StrUtil.isBlank(baseUrl)) {
            log.error("[告警管理] 无法解析Alertmanager地址");
            return null;
        }
        Instant now = Instant.now();
        Instant end = now.plus(days, ChronoUnit.DAYS);
        Map<String, Object> payload = new HashMap<>();
        List<Map<String, Object>> matchers = new ArrayList<>();
        matchers.add(matcher("alertname", record.getAlertName()));
        matchers.add(matcher("instance", record.getInstance()));
        payload.put("matchers", matchers);
        payload.put("startsAt", now.toString());
        payload.put("endsAt", end.toString());
        payload.put("createdBy", resolveOperator());
        payload.put("comment", StrUtil.blankToDefault(message, "控制台抑制"));
        String result = postJson(baseUrl + "/api/v2/silences", JSONUtil.toJsonStr(payload));
        if (StrUtil.isBlank(result)) {
            log.error("[告警管理] 创建静默失败, 响应为空");
            return null;
        }
        try {
            Map<String, Object> response = JSONUtil.toBean(result, new TypeReference<Map<String, Object>>() {}, false);
            Object silenceId = response.get("silenceID");
            if (silenceId == null) {
                log.error("[告警管理] 创建静默失败, 响应中无silenceID, response={}", result);
                return null;
            }
            log.info("[告警管理] 创建静默成功, silenceID={}", silenceId);
            return String.valueOf(silenceId);
        } catch (Exception e) {
            log.error("[告警管理] 解析silence响应失败, response={}", result, e);
            return null;
        }
    }

    public void sendResolvedAlert(AlertRecordEntity record, String message) {
        log.info("[告警管理] 发送恢复告警, alertName={}, instance={}",
                 record != null ? record.getAlertName() : null,
                 record != null ? record.getInstance() : null);
        if (record == null) {
            log.warn("[告警管理] 发送恢复告警参数为空");
            return;
        }
        String baseUrl = resolveAlertmanagerBaseUrl();
        if (StrUtil.isBlank(baseUrl)) {
            log.error("[告警管理] 无法解析Alertmanager地址");
            return;
        }
        Map<String, Object> labels = new HashMap<>();
        labels.put("alertname", record.getAlertName());
        labels.put("instance", record.getInstance());
        labels.put("severity", "recover");

        Map<String, Object> annotations = new HashMap<>();
        annotations.put("summary", StrUtil.blankToDefault(record.getSummary(), "告警已关闭"));
        annotations.put("description", StrUtil.blankToDefault(message, "控制台手动关闭告警"));

        Map<String, Object> alert = new HashMap<>();
        alert.put("labels", labels);
        alert.put("annotations", annotations);
        Instant startsAt = record.getStartsAt() == null ? Instant.now().minus(1, ChronoUnit.MINUTES) : record.getStartsAt().toInstant();
        alert.put("startsAt", startsAt.toString());
        alert.put("endsAt", Instant.now().toString());

        List<Map<String, Object>> alerts = new ArrayList<>();
        alerts.add(alert);
        String result = postJson(baseUrl + "/api/v2/alerts", JSONUtil.toJsonStr(alerts));
        if (StrUtil.isNotBlank(result)) {
            log.info("[告警管理] 发送恢复告警成功");
        }
    }

    private Map<String, Object> matcher(String name, String value) {
        Map<String, Object> matcher = new HashMap<>();
        matcher.put("name", name);
        matcher.put("value", StrUtil.nullToEmpty(value));
        matcher.put("isRegex", false);
        matcher.put("isEqual", true);
        return matcher;
    }

    private String resolveAlertmanagerBaseUrl() {
        List<MonitorComponentEntity> list = monitorComponentMapper.selectList(
            new LambdaQueryWrapper<MonitorComponentEntity>()
                .eq(MonitorComponentEntity::getType, TYPE_ALERTMANAGER)
                .select(MonitorComponentEntity::getWebUrl, MonitorComponentEntity::getIp,
                    MonitorComponentEntity::getPort, MonitorComponentEntity::getType)
                .last("LIMIT 10")
        );
        if (list != null) {
            for (MonitorComponentEntity component : list) {
                String base = buildBaseUrl(component);
                if (StrUtil.isNotBlank(base)) {
                    return base;
                }
            }
        }
        return defaultAlertmanagerUrl;
    }

    private String buildBaseUrl(MonitorComponentEntity entity) {
        if (entity == null) {
            return null;
        }
        if (StrUtil.isNotBlank(entity.getWebUrl())) {
            return trimRight(entity.getWebUrl());
        }
        if (StrUtil.isBlank(entity.getIp())) {
            return null;
        }
        if (entity.getPort() == null) {
            return "http://" + entity.getIp().trim();
        }
        return "http://" + entity.getIp().trim() + ":" + entity.getPort();
    }

    private String trimRight(String url) {
        String value = url.trim();
        while (value.endsWith("/")) {
            value = value.substring(0, value.length() - 1);
        }
        return value;
    }

    private String resolveOperator() {
        String username = SecurityUser.getUser().getUsername();
        return StrUtil.isBlank(username) ? "system" : username;
    }

    private String postJson(String target, String body) {
        HttpURLConnection connection = null;
        long startTime = System.currentTimeMillis();
        try {
            log.debug("[告警管理] HTTP请求, url={}", target);
            connection = (HttpURLConnection) new URL(target).openConnection();
            connection.setRequestMethod("POST");
            connection.setConnectTimeout(httpTimeoutConfig.getConnectTimeout());
            connection.setReadTimeout(httpTimeoutConfig.getReadTimeout());
            connection.setDoOutput(true);
            connection.setRequestProperty("Content-Type", "application/json");
            try (OutputStream out = connection.getOutputStream()) {
                out.write(StrUtil.nullToEmpty(body).getBytes(StandardCharsets.UTF_8));
                out.flush();
            }
            int code = connection.getResponseCode();
            long elapsedTime = System.currentTimeMillis() - startTime;
            if (code >= 200 && code < 300) {
                try (InputStream in = connection.getInputStream()) {
                    String response = new String(in.readAllBytes(), StandardCharsets.UTF_8);
                    log.info("[告警管理] HTTP请求成功, code={}, 耗时={}ms", code, elapsedTime);
                    return response;
                }
            }
            log.warn("[告警管理] HTTP请求失败, code={}, 耗时={}ms", code, elapsedTime);
            return null;
        } catch (Exception e) {
            long elapsedTime = System.currentTimeMillis() - startTime;
            log.error("[告警管理] HTTP请求异常, 耗时={}ms", elapsedTime, e);
            return null;
        } finally {
            if (connection != null) {
                connection.disconnect();
            }
        }
    }
}
