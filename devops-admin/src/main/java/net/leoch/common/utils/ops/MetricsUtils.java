package net.leoch.common.utils.ops;

import cn.hutool.core.util.StrUtil;
import lombok.extern.slf4j.Slf4j;

import java.net.HttpURLConnection;
import java.net.URL;

@Slf4j
public final class MetricsUtils {
    private MetricsUtils() {}

    public static boolean metricsOk(String instance, int timeoutMs) {
        String url = buildMetricsUrl(instance);
        if (StrUtil.isBlank(url)) {
            return false;
        }
        HttpURLConnection connection = null;
        try {
            connection = (HttpURLConnection) new URL(url).openConnection();
            connection.setRequestMethod("GET");
            connection.setConnectTimeout(timeoutMs);
            connection.setReadTimeout(timeoutMs);
            return connection.getResponseCode() == 200;
        } catch (Exception e) {
            log.debug("[Metrics检查] 探测失败, url={}", url, e);
            return false;
        } finally {
            if (connection != null) {
                connection.disconnect();
            }
        }
    }

    public static String buildMetricsUrl(String instance) {
        String base = instance == null ? "" : instance.trim();
        if (StrUtil.isBlank(base)) {
            return null;
        }
        if (!base.startsWith("http://") && !base.startsWith("https://")) {
            base = "http://" + base;
        }
        if (base.contains("/metrics")) {
            return base;
        }
        if (base.endsWith("/")) {
            return base + "metrics";
        }
        return base + "/metrics";
    }
}
