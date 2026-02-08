package net.leoch.modules.ops.service;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.fasterxml.jackson.core.type.TypeReference;
import net.leoch.common.utils.JsonUtils;
import net.leoch.modules.ops.config.ZabbixConfig;
import lombok.extern.slf4j.Slf4j;
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
    private final IZabbixConfigService configService;

    public ZabbixClient(IZabbixConfigService configService) {
        this.configService = configService;
    }

    public List<Map<String, String>> getHostsByTemplates() {
        ZabbixConfig config = configService.getConfig();
        if (config == null || StrUtil.isBlank(config.getUrl())) {
            return new ArrayList<>();
        }
        String auth = login(config);
        if (StrUtil.isBlank(auth)) {
            return new ArrayList<>();
        }
        List<String> templateIds = getTemplateIds(config, auth);
        if (CollUtil.isEmpty(templateIds)) {
            return new ArrayList<>();
        }
        return getHosts(config, auth, templateIds);
    }

    private String login(ZabbixConfig config) {
        Map<String, Object> params = new HashMap<>();
        params.put("username", config.getUsername());
        params.put("password", config.getPassword());
        Object result = call(config, "user.login", params, null);
        return result == null ? null : String.valueOf(result);
    }

    private List<String> getTemplateIds(ZabbixConfig config, String auth) {
        if (CollUtil.isEmpty(config.getTemplates())) {
            return new ArrayList<>();
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
        try {
            URL url = new URL(config.getUrl());
            connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("POST");
            connection.setConnectTimeout(5000);
            connection.setReadTimeout(10000);
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
            String body = JsonUtils.toJsonString(payload);
            try (OutputStream out = connection.getOutputStream()) {
                out.write(body.getBytes(StandardCharsets.UTF_8));
            }
            int code = connection.getResponseCode();
            if (code != 200) {
                log.warn("[Zabbix] 接口响应异常, code={}", code);
                return null;
            }
            byte[] bytes = connection.getInputStream().readAllBytes();
            Map<String, Object> resp = JsonUtils.parseObject(new String(bytes, StandardCharsets.UTF_8), new TypeReference<Map<String, Object>>() {});
            if (resp == null) {
                return null;
            }
            if (resp.get("error") != null) {
                log.warn("[Zabbix] 接口返回错误: {}", resp.get("error"));
                return null;
            }
            return resp.get("result");
        } catch (Exception e) {
            log.warn("[Zabbix] 接口调用失败", e);
            return null;
        } finally {
            if (connection != null) {
                connection.disconnect();
            }
        }
    }
}
