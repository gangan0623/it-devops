package net.leoch.modules.ops.service;

import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONArray;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.framework.config.ops.OnlineStatusConfig;
import org.springframework.stereotype.Service;

import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

@Slf4j
@Service
@RequiredArgsConstructor
public class PrometheusOnlineStatusService {
    public static final String JOB_WINDOWS = "windows";
    public static final String JOB_LINUX = "linux";
    public static final String JOB_HTTP_PROBE = "http_probe";

    private final OnlineStatusConfig config;

    public BatchStatusResult queryJobStatus(String job) {
        if (StrUtil.isBlank(job)) {
            return BatchStatusResult.failure();
        }
        String body = executeQuery("up{job=\"" + escapePromQl(job) + "\"}");
        if (body == null) {
            return BatchStatusResult.failure();
        }
        return BatchStatusResult.success(parseStatusMap(body));
    }

    public boolean isOnline(String job, String instance) {
        String normalizedInstance = normalizeInstance(instance);
        if (StrUtil.isBlank(job) || StrUtil.isBlank(normalizedInstance)) {
            return false;
        }
        String body = executeQuery("up{job=\"" + escapePromQl(job) + "\",instance=\"" + escapePromQl(normalizedInstance) + "\"}");
        if (body == null) {
            return false;
        }
        Map<String, Boolean> statusMap = parseStatusMap(body);
        Boolean status = statusMap.get(normalizedInstance);
        return Boolean.TRUE.equals(status);
    }

    private Map<String, Boolean> parseStatusMap(String body) {
        if (StrUtil.isBlank(body) || !JSONUtil.isTypeJSON(body)) {
            return Collections.emptyMap();
        }
        try {
            JSONObject root = JSONUtil.parseObj(body);
            if (!"success".equalsIgnoreCase(root.getStr("status"))) {
                return Collections.emptyMap();
            }
            JSONObject data = root.getJSONObject("data");
            if (data == null) {
                return Collections.emptyMap();
            }
            JSONArray result = data.getJSONArray("result");
            if (result == null || result.isEmpty()) {
                return Collections.emptyMap();
            }
            Map<String, Boolean> statusMap = new LinkedHashMap<>();
            for (Object item : result) {
                if (!(item instanceof JSONObject row)) {
                    continue;
                }
                JSONObject metric = row.getJSONObject("metric");
                if (metric == null) {
                    continue;
                }
                String instance = normalizeInstance(metric.getStr("instance"));
                if (StrUtil.isBlank(instance)) {
                    continue;
                }
                JSONArray value = row.getJSONArray("value");
                if (value == null || value.size() < 2) {
                    continue;
                }
                String rawValue = String.valueOf(value.get(1));
                statusMap.put(instance, "1".equals(rawValue) || "1.0".equals(rawValue));
            }
            return statusMap;
        } catch (Exception e) {
            log.warn("[Prometheus在线状态] 解析响应失败", e);
            return Collections.emptyMap();
        }
    }

    private String executeQuery(String promQl) {
        String baseUrl = config.getPrometheus().getQueryBaseUrl();
        if (StrUtil.isBlank(baseUrl) || StrUtil.isBlank(promQl)) {
            return null;
        }
        HttpURLConnection conn = null;
        try {
            String encoded = URLEncoder.encode(promQl, StandardCharsets.UTF_8);
            URL url = new URL(baseUrl + (baseUrl.contains("?") ? "&" : "?") + "query=" + encoded);
            conn = (HttpURLConnection) url.openConnection();
            conn.setRequestMethod("GET");
            conn.setConnectTimeout(config.getPrometheus().getConnectTimeout());
            conn.setReadTimeout(config.getPrometheus().getReadTimeout());
            conn.connect();
            if (conn.getResponseCode() != 200) {
                log.warn("[Prometheus在线状态] 查询失败, code={}, promQl={}", conn.getResponseCode(), promQl);
                return null;
            }
            try (InputStream in = conn.getInputStream()) {
                return new String(in.readAllBytes(), StandardCharsets.UTF_8);
            }
        } catch (Exception e) {
            log.warn("[Prometheus在线状态] 查询异常, promQl={}", promQl, e);
            return null;
        } finally {
            if (conn != null) {
                conn.disconnect();
            }
        }
    }

    private String normalizeInstance(String instance) {
        String value = StrUtil.trim(instance);
        if (StrUtil.isBlank(value)) {
            return null;
        }
        if (StrUtil.startWithIgnoreCase(value, "http://")) {
            value = value.substring(7);
        } else if (StrUtil.startWithIgnoreCase(value, "https://")) {
            value = value.substring(8);
        }
        int slashIndex = value.indexOf('/');
        if (slashIndex >= 0) {
            value = value.substring(0, slashIndex);
        }
        return StrUtil.trim(value);
    }

    private String escapePromQl(String value) {
        return StrUtil.nullToEmpty(value).replace("\\", "\\\\").replace("\"", "\\\"");
    }

    public record BatchStatusResult(boolean success, Map<String, Boolean> statusMap) {
        public static BatchStatusResult success(Map<String, Boolean> statusMap) {
            return new BatchStatusResult(true, statusMap == null ? Collections.emptyMap() : statusMap);
        }

        public static BatchStatusResult failure() {
            return new BatchStatusResult(false, Collections.emptyMap());
        }
    }
}
