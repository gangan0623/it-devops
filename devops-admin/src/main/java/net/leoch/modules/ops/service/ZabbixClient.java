package net.leoch.modules.ops.service;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.lang.TypeReference;
import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONUtil;
import lombok.extern.slf4j.Slf4j;
import net.leoch.framework.config.ops.HttpTimeoutConfig;
import net.leoch.framework.config.ops.ZabbixConfig;
import org.springframework.stereotype.Service;

import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Zabbix API client
 */
@Slf4j
@Service
public class ZabbixClient {
    private final ZabbixConfigService configService;
    private final HttpTimeoutConfig httpTimeoutConfig;

    public ZabbixClient(ZabbixConfigService configService, HttpTimeoutConfig httpTimeoutConfig) {
        this.configService = configService;
        this.httpTimeoutConfig = httpTimeoutConfig;
    }

    public List<Map<String, String>> getHostsByTemplates() {
        log.info("[Zabbix] 开始获取主机列表");
        ZabbixConfig config = configService.getConfig();
        if (config == null || StrUtil.isBlank(config.getUrl())) {
            log.warn("[Zabbix] 配置为空或URL未配置");
            return List.of();
        }
        String auth = login(config);
        if (StrUtil.isBlank(auth)) {
            log.warn("[Zabbix] 登录失败");
            return List.of();
        }
        List<String> templateIds = getTemplateIds(config, auth);
        if (CollUtil.isEmpty(templateIds)) {
            log.warn("[Zabbix] 未找到匹配的模板, templates={}", config.getTemplates());
            return List.of();
        }
        List<Map<String, String>> hosts = getHosts(config, auth, templateIds);
        log.info("[Zabbix] 获取主机列表完成, 数量={}", hosts.size());
        return hosts;
    }

    private String login(ZabbixConfig config) {
        log.debug("[Zabbix] 开始登录, url={}, username={}", config.getUrl(), config.getUsername());
        Map<String, Object> params = new HashMap<>();
        params.put("username", config.getUsername());
        params.put("password", config.getPassword());
        Object result = call(config, "user.login", params, null);
        if (result == null) {
            log.error("[Zabbix] 登录失败, url={}, username={}", config.getUrl(), config.getUsername());
            return null;
        }
        return String.valueOf(result);
    }

    private List<String> getTemplateIds(ZabbixConfig config, String auth) {
        if (CollUtil.isEmpty(config.getTemplates())) {
            return List.of();
        }
        Map<String, Object> params = new HashMap<>();
        params.put("output", List.of("templateid", "name"));
        Map<String, Object> filter = new HashMap<>();
        filter.put("name", config.getTemplates());
        params.put("filter", filter);
        Object result = call(config, "template.get", params, auth);
        List<String> ids = new ArrayList<>();
        if (result instanceof List<?> list) {
            for (Object item : list) {
                if (!(item instanceof Map<?, ?> map)) {
                    continue;
                }
                Object id = map.get("templateid");
                if (id != null) {
                    ids.add(String.valueOf(id));
                }
            }
        }
        return ids;
    }

    private List<Map<String, String>> getHosts(ZabbixConfig config, String auth, List<String> templateIds) {
        Map<String, Object> params = new HashMap<>();
        params.put("output", List.of("host", "name"));
        params.put("templateids", templateIds);
        params.put("selectInterfaces", List.of("ip"));
        Object result = call(config, "host.get", params, auth);
        List<Map<String, String>> hosts = new ArrayList<>();
        if (result instanceof List<?> list) {
            for (Object item : list) {
                if (!(item instanceof Map<?, ?> map)) {
                    continue;
                }
                String name = String.valueOf(map.get("host"));
                String ip = "";
                Object interfaces = map.get("interfaces");
                if (interfaces instanceof List<?> ifaceList && !ifaceList.isEmpty()) {
                    Object first = ifaceList.get(0);
                    if (first instanceof Map<?, ?> ifaceMap) {
                        Object ipVal = ifaceMap.get("ip");
                        if (ipVal != null) {
                            ip = String.valueOf(ipVal);
                        }
                    }
                }
                if (StrUtil.isBlank(ip)) {
                    continue;
                }
                Map<String, String> host = new HashMap<>();
                host.put("ip", ip);
                host.put("name", name);
                hosts.add(host);
            }
        }
        return hosts;
    }

    private Object call(ZabbixConfig config, String method, Map<String, Object> params, String bearer) {
        HttpURLConnection connection = null;
        long startTime = System.currentTimeMillis();
        try {
            // 脱敏处理：避免在日志中泄露敏感信息
            Map<String, Object> safeParams = new HashMap<>(params);
            if (safeParams.containsKey("password")) {
                safeParams.put("password", "******");
            }
            log.debug("[Zabbix] 开始调用接口, method={}, params={}", method, JSONUtil.toJsonStr(safeParams));
            URL url = new URL(config.getUrl());
            connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("POST");
            connection.setConnectTimeout(httpTimeoutConfig.getConnectTimeout());
            connection.setReadTimeout(httpTimeoutConfig.getReadTimeout());
            connection.setDoOutput(true);
            connection.setRequestProperty("Content-Type", "application/json");
            if (StrUtil.isNotBlank(bearer)) {
                connection.setRequestProperty("Authorization", "Bearer " + bearer);
            }
            Map<String, Object> payload = new HashMap<>();
            payload.put("jsonrpc", "2.0");
            payload.put("method", method);
            payload.put("params", params);
            payload.put("id", 1);
            String body = JSONUtil.toJsonStr(payload);
            try (OutputStream out = connection.getOutputStream()) {
                out.write(body.getBytes(StandardCharsets.UTF_8));
            }
            int code = connection.getResponseCode();
            if (code != 200) {
                log.warn("[Zabbix] 接口响应异常, method={}, code={}", method, code);
                return null;
            }
            byte[] bytes = connection.getInputStream().readAllBytes();
            Map<String, Object> resp = JSONUtil.toBean(new String(bytes, StandardCharsets.UTF_8), new TypeReference<>() {
            }, false);
            if (resp == null) {
                log.warn("[Zabbix] 接口响应为空, method={}", method);
                return null;
            }
            if (resp.get("error") != null) {
                log.error("[Zabbix] 接口返回错误, method={}, error={}", method, resp.get("error"));
                return null;
            }
            long elapsedTime = System.currentTimeMillis() - startTime;
            log.info("[Zabbix] 接口调用成功, method={}, 耗时={}ms", method, elapsedTime);
            return resp.get("result");
        } catch (Exception e) {
            long elapsedTime = System.currentTimeMillis() - startTime;
            log.error("[Zabbix] 接口调用失败, method={}, 耗时={}ms", method, elapsedTime, e);
            return null;
        } finally {
            if (connection != null) {
                connection.disconnect();
            }
        }
    }
}
