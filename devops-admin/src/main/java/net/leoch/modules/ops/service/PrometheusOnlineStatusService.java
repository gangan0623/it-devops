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
import java.net.URI;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

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
        if (StrUtil.isBlank(job) || StrUtil.isBlank(instance)) {
            return false;
        }
        BatchStatusResult result = queryJobStatus(job);
        if (!result.success()) {
            return false;
        }
        return matchesOnline(result.statusMap(), job, instance);
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
                String instance = StrUtil.trim(metric.getStr("instance"));
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

    private String escapePromQl(String value) {
        return StrUtil.nullToEmpty(value).replace("\\", "\\\\").replace("\"", "\\\"");
    }

    public boolean matchesOnline(Map<String, Boolean> statusMap, String job, String instance) {
        if (statusMap == null || statusMap.isEmpty() || StrUtil.isBlank(job) || StrUtil.isBlank(instance)) {
            return false;
        }
        Set<String> expectedKeys = buildMatchKeys(job, instance);
        if (expectedKeys.isEmpty()) {
            return false;
        }
        for (Map.Entry<String, Boolean> entry : statusMap.entrySet()) {
            if (!Boolean.TRUE.equals(entry.getValue())) {
                continue;
            }
            Set<String> actualKeys = buildMatchKeys(job, entry.getKey());
            for (String key : actualKeys) {
                if (expectedKeys.contains(key)) {
                    return true;
                }
            }
        }
        return false;
    }

    private Set<String> buildMatchKeys(String job, String instance) {
        String value = StrUtil.trim(instance);
        if (StrUtil.isBlank(value)) {
            return Collections.emptySet();
        }
        return JOB_HTTP_PROBE.equalsIgnoreCase(job) ? buildHttpProbeKeys(value) : buildHostKeys(value);
    }

    private Set<String> buildHostKeys(String instance) {
        LinkedHashSet<String> keys = new LinkedHashSet<>();
        String trimmed = StrUtil.trim(instance);
        if (StrUtil.isBlank(trimmed)) {
            return keys;
        }
        keys.add(trimmed);
        String withoutScheme = StrUtil.removePrefixIgnoreCase(StrUtil.removePrefixIgnoreCase(trimmed, "http://"), "https://");
        if (StrUtil.isNotBlank(withoutScheme)) {
            keys.add(withoutScheme);
        }
        String withoutPath = substringBeforeSlash(withoutScheme);
        if (StrUtil.isNotBlank(withoutPath)) {
            keys.add(withoutPath);
            String hostOnly = substringBeforeColon(withoutPath);
            if (StrUtil.isNotBlank(hostOnly)) {
                keys.add(hostOnly);
            }
        }
        return keys;
    }

    private Set<String> buildHttpProbeKeys(String instance) {
        LinkedHashSet<String> keys = new LinkedHashSet<>();
        String trimmed = StrUtil.trim(instance);
        if (StrUtil.isBlank(trimmed)) {
            return keys;
        }
        keys.add(trimmed);
        keys.add(removeTrailingSlash(trimmed));
        try {
            URI uri = URI.create(trimmed);
            String scheme = StrUtil.blankToDefault(uri.getScheme(), "http");
            String host = uri.getHost();
            int port = uri.getPort();
            String path = StrUtil.blankToDefault(uri.getPath(), "");
            String base = host == null ? trimmed : scheme + "://" + host + (port > 0 ? ":" + port : "");
            keys.add(base);
            keys.add(removeTrailingSlash(base + path));
            if (host != null) {
                keys.add(host);
                if (port > 0) {
                    keys.add(host + ":" + port);
                }
            }
        } catch (Exception ignore) {
            keys.add(substringBeforeSlash(StrUtil.removePrefixIgnoreCase(StrUtil.removePrefixIgnoreCase(trimmed, "http://"), "https://")));
        }
        keys.removeIf(StrUtil::isBlank);
        return keys;
    }

    private String substringBeforeSlash(String value) {
        if (StrUtil.isBlank(value)) {
            return value;
        }
        int slashIndex = value.indexOf('/');
        return slashIndex >= 0 ? value.substring(0, slashIndex) : value;
    }

    private String substringBeforeColon(String value) {
        if (StrUtil.isBlank(value)) {
            return value;
        }
        int colonIndex = value.indexOf(':');
        return colonIndex >= 0 ? value.substring(0, colonIndex) : value;
    }

    private String removeTrailingSlash(String value) {
        if (StrUtil.isBlank(value)) {
            return value;
        }
        String result = value;
        while (result.endsWith("/")) {
            result = result.substring(0, result.length() - 1);
        }
        return result;
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
