package net.leoch.modules.ops.service;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.lang.TypeReference;
import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONUtil;
import lombok.extern.slf4j.Slf4j;
import net.leoch.framework.config.ops.HttpTimeoutConfig;
import net.leoch.framework.config.ops.ZabbixConfig;
import net.leoch.modules.sys.vo.rsp.ZabbixHostGroupRsp;
import org.springframework.stereotype.Service;

import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Zabbix API client
 */
@Slf4j
@Service
public class ZabbixClient {
    private static final Set<String> IF_METRIC_PREFIXES = Set.of(
            "net.if.in[",
            "net.if.out[",
            "net.if.out.errors[",
            "net.if.in.errors[",
            "net.if.out.discards[",
            "net.if.in.discards["
    );

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
        if (config.getStatus() != null && config.getStatus() == 0) {
            log.info("[Zabbix] 配置已禁用，跳过拉取");
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

    public void testConnection(ZabbixConfig config) {
        if (config == null || StrUtil.isBlank(config.getUrl())) {
            throw new IllegalArgumentException("Zabbix URL不能为空");
        }
        String auth = login(config);
        if (StrUtil.isBlank(auth)) {
            throw new IllegalStateException("Zabbix登录失败");
        }
    }

    public String getApiVersion(ZabbixConfig config) {
        if (config == null || StrUtil.isBlank(config.getUrl())) {
            return null;
        }
        Object result = call(config, "apiinfo.version", new HashMap<>(), null);
        return result == null ? null : String.valueOf(result);
    }

    public String fetchLatestVersion() {
        return fetchLatestVersionFromTags();
    }


    private String fetchLatestVersionFromTags() {
        HttpURLConnection connection = null;
        try {
            URL url = new URL("https://api.github.com/repos/zabbix/zabbix/tags?per_page=20");
            connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("GET");
            connection.setConnectTimeout(httpTimeoutConfig.getConnectTimeout());
            connection.setReadTimeout(httpTimeoutConfig.getReadTimeout());
            connection.setRequestProperty("Accept", "application/vnd.github+json");
            connection.setRequestProperty("User-Agent", "it-devops/zabbix-version-check");
            int code = connection.getResponseCode();
            if (code != 200) {
                log.warn("[Zabbix] 获取 tags 版本失败, httpCode={}", code);
                return null;
            }
            String body = new String(connection.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
            List<Object> list = JSONUtil.toBean(body, new TypeReference<>() {
            }, false);
            if (CollUtil.isEmpty(list)) {
                return null;
            }
            for (Object item : list) {
                if (!(item instanceof Map<?, ?> map)) {
                    continue;
                }
                Object name = map.get("name");
                if (name == null) {
                    continue;
                }
                String version = stripVersionPrefix(String.valueOf(name));
                if (StrUtil.isBlank(version)) {
                    continue;
                }
                String lower = version.toLowerCase();
                if (lower.contains("alpha") || lower.contains("beta") || lower.contains("rc")) {
                    continue;
                }
                return version;
            }
            // 如果前20个都是预发布，则退回第一个tag
            Object first = list.get(0);
            if (first instanceof Map<?, ?> map && map.get("name") != null) {
                return stripVersionPrefix(String.valueOf(map.get("name")));
            }
            return null;
        } catch (Exception e) {
            log.warn("[Zabbix] 获取 tags 最新版本异常", e);
            return null;
        } finally {
            if (connection != null) {
                connection.disconnect();
            }
        }
    }

    public int compareVersions(String v1, String v2) {
        String a = normalizeVersion(v1);
        String b = normalizeVersion(v2);
        String[] p1 = a.split("\\.");
        String[] p2 = b.split("\\.");
        int len = Math.max(p1.length, p2.length);
        for (int i = 0; i < len; i++) {
            int n1 = i < p1.length ? parseVersionInt(p1[i]) : 0;
            int n2 = i < p2.length ? parseVersionInt(p2[i]) : 0;
            if (n1 != n2) {
                return Integer.compare(n1, n2);
            }
        }
        return 0;
    }

    public List<ZabbixHostGroupRsp> getHostGroups(ZabbixConfig config) {
        if (config == null || StrUtil.isBlank(config.getUrl())) {
            return List.of();
        }
        String auth = login(config);
        if (StrUtil.isBlank(auth)) {
            return List.of();
        }
        Map<String, Object> params = new HashMap<>();
        params.put("output", List.of("groupid", "name"));
        params.put("sortfield", "name");
        Object result = call(config, "hostgroup.get", params, auth);
        if (!(result instanceof List<?> list)) {
            return List.of();
        }
        return list.stream()
                .filter(Objects::nonNull)
                .map(item -> {
                    if (!(item instanceof Map<?, ?> map)) {
                        return null;
                    }
                    Object groupId = map.get("groupid");
                    Object name = map.get("name");
                    if (groupId == null || name == null) {
                        return null;
                    }
                    ZabbixHostGroupRsp rsp = new ZabbixHostGroupRsp();
                    rsp.setGroupId(String.valueOf(groupId));
                    rsp.setName(String.valueOf(name));
                    return rsp;
                })
                .filter(Objects::nonNull)
                .toList();
    }

    public List<Map<String, Object>> getHostsForNetworkSync(ZabbixConfig config, List<String> hostGroupIds) {
        if (config == null || StrUtil.isBlank(config.getUrl())) {
            return List.of();
        }
        String auth = login(config);
        if (StrUtil.isBlank(auth)) {
            throw new IllegalStateException("Zabbix登录失败");
        }
        List<String> templateIds = getTemplateIds(config, auth);
        if (CollUtil.isEmpty(templateIds)) {
            return List.of();
        }
        Map<String, Object> params = new HashMap<>();
        params.put("output", List.of("hostid", "host", "name"));
        params.put("templateids", templateIds);
        if (CollUtil.isNotEmpty(hostGroupIds)) {
            params.put("groupids", hostGroupIds);
        }
        params.put("selectInterfaces", List.of("ip"));
        params.put("selectHostGroups", List.of("groupid", "name"));
        params.put("selectParentTemplates", List.of("templateid", "name"));
        Object result = call(config, "host.get", params, auth);
        if (!(result instanceof List<?> list)) {
            return List.of();
        }
        List<Map<String, Object>> rows = new ArrayList<>();
        for (Object item : list) {
            if (!(item instanceof Map<?, ?> map)) {
                continue;
            }
            Map<String, Object> row = new HashMap<>();
            for (Map.Entry<?, ?> entry : map.entrySet()) {
                if (entry.getKey() != null) {
                    row.put(String.valueOf(entry.getKey()), entry.getValue());
                }
            }
            rows.add(row);
        }
        return rows;
    }

    public Map<String, Map<String, Map<String, Object>>> getIcmpItemsByHostIds(ZabbixConfig config, List<String> hostIds) {
        if (config == null || StrUtil.isBlank(config.getUrl()) || CollUtil.isEmpty(hostIds)) {
            return Map.of();
        }
        String auth = login(config);
        if (StrUtil.isBlank(auth)) {
            throw new IllegalStateException("Zabbix登录失败");
        }
        Map<String, Object> params = new HashMap<>();
        params.put("output", List.of("itemid", "hostid", "name", "key_", "lastvalue", "units", "lastclock"));
        params.put("hostids", hostIds);
        params.put("filter", Map.of("key_", List.of("icmpping", "icmppingloss", "icmppingsec")));
        Object result = call(config, "item.get", params, auth);
        if (!(result instanceof List<?> list)) {
            return Map.of();
        }
        Map<String, Map<String, Map<String, Object>>> hostItemMap = new HashMap<>();
        for (Object item : list) {
            if (!(item instanceof Map<?, ?> map)) {
                continue;
            }
            String hostId = map.get("hostid") == null ? null : String.valueOf(map.get("hostid"));
            String key = map.get("key_") == null ? null : String.valueOf(map.get("key_"));
            if (StrUtil.isBlank(hostId) || StrUtil.isBlank(key)) {
                continue;
            }
            Map<String, Object> normalized = new HashMap<>();
            for (Map.Entry<?, ?> entry : map.entrySet()) {
                if (entry.getKey() != null) {
                    normalized.put(String.valueOf(entry.getKey()), entry.getValue());
                }
            }
            hostItemMap.computeIfAbsent(hostId, k -> new HashMap<>()).put(key, normalized);
        }
        return hostItemMap;
    }

    public List<Map<String, Object>> getInterfaceItemsByHostIp(ZabbixConfig config, String hostIp) {
        if (config == null || StrUtil.isBlank(config.getUrl()) || StrUtil.isBlank(hostIp)) {
            return List.of();
        }
        String auth = login(config);
        if (StrUtil.isBlank(auth)) {
            throw new IllegalStateException("Zabbix登录失败");
        }

        Map<String, Object> ifaceParams = new HashMap<>();
        ifaceParams.put("output", List.of("interfaceid", "hostid", "ip"));
        ifaceParams.put("filter", Map.of("ip", List.of(hostIp)));
        Object ifaceResult = call(config, "hostinterface.get", ifaceParams, auth);
        if (!(ifaceResult instanceof List<?> ifaceList) || ifaceList.isEmpty()) {
            return List.of();
        }
        List<String> hostIds = ifaceList.stream()
                .filter(Map.class::isInstance)
                .map(Map.class::cast)
                .map(map -> map.get("hostid"))
                .filter(Objects::nonNull)
                .map(String::valueOf)
                .distinct()
                .toList();
        if (hostIds.isEmpty()) {
            return List.of();
        }

        // 第一步：先通过 item tags 抽取接口索引集合
        List<Map<String, Object>> taggedItems = getInterfaceTaggedItems(config, auth, hostIds);
        Set<String> taggedIndexes = new java.util.HashSet<>();
        for (Map<String, Object> row : taggedItems) {
            String key = row.get("key_") == null ? "" : String.valueOf(row.get("key_")).toLowerCase();
            String idx = extractInterfaceIndex(key);
            if (StrUtil.isBlank(idx)) {
                continue;
            }
            if (hasAnyTag(row.get("tags"))) {
                taggedIndexes.add(idx);
            }
        }

        // 第二步：按接口 item 查询，再按第一步索引过滤
        Map<String, Object> itemParams = new HashMap<>();
        itemParams.put("output", List.of("itemid", "hostid", "name", "key_", "lastvalue", "units", "lastclock"));
        itemParams.put("selectTags", "extend");
        itemParams.put("hostids", hostIds);
        itemParams.put("search", Map.of("key_", "if"));
        itemParams.put("searchByAny", true);
        Object itemResult = call(config, "item.get", itemParams, auth);
        if (!(itemResult instanceof List<?> itemList)) {
            return List.of();
        }
        List<Map<String, Object>> rows = new ArrayList<>();
        for (Object item : itemList) {
            if (!(item instanceof Map<?, ?> map)) {
                continue;
            }
            Object keyRaw = map.get("key_");
            String key = keyRaw == null ? "" : String.valueOf(keyRaw).toLowerCase();
            String idx = extractInterfaceIndex(key);
            if (!taggedIndexes.isEmpty() && StrUtil.isNotBlank(idx) && !taggedIndexes.contains(idx)) {
                continue;
            }
            Map<String, Object> row = new HashMap<>();
            for (Map.Entry<?, ?> entry : map.entrySet()) {
                if (entry.getKey() != null) {
                    row.put(String.valueOf(entry.getKey()), entry.getValue());
                }
            }
            rows.add(row);
        }
        return rows;
    }

    public Map<String, List<Map<String, Object>>> getInterfaceMetricHistoryByHostIp(ZabbixConfig config,
                                                                                      String hostIp,
                                                                                      String interfaceIndex,
                                                                                      long timeFrom,
                                                                                      long timeTill) {
        if (config == null || StrUtil.isBlank(config.getUrl()) || StrUtil.isBlank(hostIp) || StrUtil.isBlank(interfaceIndex)) {
            return Map.of();
        }
        String auth = login(config);
        if (StrUtil.isBlank(auth)) {
            throw new IllegalStateException("Zabbix登录失败");
        }

        List<String> hostIds = getHostIdsByIp(config, auth, hostIp);
        if (hostIds.isEmpty()) {
            return Map.of();
        }

        // 先用 tags 判定接口是否存在，避免接口索引误匹配
        List<Map<String, Object>> taggedItems = getInterfaceTaggedItems(config, auth, hostIds);
        boolean existsByTag = taggedItems.stream()
                .map(row -> row.get("key_"))
                .filter(Objects::nonNull)
                .map(String::valueOf)
                .map(String::toLowerCase)
                .map(this::extractInterfaceIndex)
                .anyMatch(interfaceIndex::equals);
        if (!existsByTag) {
            return Map.of();
        }

        List<Map<String, Object>> items = getInterfaceMetricItems(config, auth, hostIds, interfaceIndex);
        if (items.isEmpty()) {
            return Map.of();
        }

        Map<String, ItemRef> metricItemMap = buildMetricItemMap(items, interfaceIndex);
        if (metricItemMap.isEmpty()) {
            return Map.of();
        }

        Map<String, List<Map<String, Object>>> historyMap = new LinkedHashMap<>();
        for (Map.Entry<String, ItemRef> entry : metricItemMap.entrySet()) {
            List<Map<String, Object>> rows = getItemHistoryRows(config, auth, entry.getValue(), timeFrom, timeTill);
            historyMap.put(entry.getKey(), rows);
        }
        return historyMap;
    }

    private List<String> getHostIdsByIp(ZabbixConfig config, String auth, String hostIp) {
        Map<String, Object> ifaceParams = new HashMap<>();
        ifaceParams.put("output", List.of("interfaceid", "hostid", "ip"));
        ifaceParams.put("filter", Map.of("ip", List.of(hostIp)));
        Object ifaceResult = call(config, "hostinterface.get", ifaceParams, auth);
        if (!(ifaceResult instanceof List<?> ifaceList) || ifaceList.isEmpty()) {
            return List.of();
        }
        return ifaceList.stream()
                .filter(Map.class::isInstance)
                .map(Map.class::cast)
                .map(map -> map.get("hostid"))
                .filter(Objects::nonNull)
                .map(String::valueOf)
                .distinct()
                .toList();
    }

    private List<Map<String, Object>> getInterfaceMetricItems(ZabbixConfig config,
                                                              String auth,
                                                              List<String> hostIds,
                                                              String interfaceIndex) {
        Map<String, Object> itemParams = new HashMap<>();
        itemParams.put("output", List.of("itemid", "hostid", "name", "key_", "lastvalue", "units", "lastclock", "value_type"));
        itemParams.put("hostids", hostIds);
        itemParams.put("search", Map.of("key_", "net.if."));
        itemParams.put("searchByAny", true);
        itemParams.put("sortfield", "itemid");
        itemParams.put("sortorder", "DESC");
        Object itemResult = call(config, "item.get", itemParams, auth);
        if (!(itemResult instanceof List<?> itemList)) {
            return List.of();
        }
        List<Map<String, Object>> rows = new ArrayList<>();
        for (Object item : itemList) {
            if (!(item instanceof Map<?, ?> map)) {
                continue;
            }
            String key = map.get("key_") == null ? "" : String.valueOf(map.get("key_")).toLowerCase();
            if (StrUtil.isBlank(key) || !isNeedMetric(key)) {
                continue;
            }
            String idx = extractInterfaceIndex(key);
            if (!StrUtil.equals(idx, interfaceIndex)) {
                continue;
            }
            rows.add(normalizeMap(map));
        }
        return rows;
    }

    private List<Map<String, Object>> getInterfaceTaggedItems(ZabbixConfig config, String auth, List<String> hostIds) {
        Map<String, Object> itemParams = new HashMap<>();
        itemParams.put("output", List.of("itemid", "hostid", "name", "key_", "lastvalue", "units", "lastclock", "value_type"));
        itemParams.put("selectTags", "extend");
        itemParams.put("hostids", hostIds);
        itemParams.put("search", Map.of("key_", "net.if."));
        itemParams.put("searchByAny", true);
        itemParams.put("sortfield", "itemid");
        itemParams.put("sortorder", "DESC");
        Object itemResult = call(config, "item.get", itemParams, auth);
        if (!(itemResult instanceof List<?> itemList)) {
            return List.of();
        }
        List<Map<String, Object>> rows = new ArrayList<>();
        for (Object item : itemList) {
            if (!(item instanceof Map<?, ?> map)) {
                continue;
            }
            rows.add(normalizeMap(map));
        }
        return rows;
    }

    private Map<String, ItemRef> buildMetricItemMap(List<Map<String, Object>> items, String interfaceIndex) {
        Map<String, ItemRef> map = new LinkedHashMap<>();
        for (Map<String, Object> item : items) {
            String key = item.get("key_") == null ? "" : String.valueOf(item.get("key_")).toLowerCase();
            if (StrUtil.isBlank(key)) {
                continue;
            }
            String idx = extractInterfaceIndex(key);
            if (!StrUtil.equals(idx, interfaceIndex)) {
                continue;
            }
            String metricKey = toMetricKey(key);
            if (StrUtil.isBlank(metricKey) || map.containsKey(metricKey)) {
                continue;
            }
            ItemRef ref = new ItemRef();
            ref.itemId = String.valueOf(item.get("itemid"));
            ref.valueType = parseInt(item.get("value_type"), 3);
            map.put(metricKey, ref);
        }
        return map;
    }

    private List<Map<String, Object>> getItemHistoryRows(ZabbixConfig config, String auth, ItemRef itemRef, long timeFrom, long timeTill) {
        if (itemRef == null || StrUtil.isBlank(itemRef.itemId)) {
            return List.of();
        }
        Map<String, Object> historyParams = new HashMap<>();
        historyParams.put("output", List.of("clock", "value"));
        historyParams.put("history", itemRef.valueType);
        historyParams.put("itemids", List.of(itemRef.itemId));
        historyParams.put("time_from", timeFrom);
        historyParams.put("time_till", timeTill);
        historyParams.put("sortfield", "clock");
        historyParams.put("sortorder", "ASC");
        Object historyResult = call(config, "history.get", historyParams, auth);
        List<Map<String, Object>> rows = normalizeRows(historyResult);
        if (!rows.isEmpty()) {
            return rows;
        }

        Map<String, Object> trendParams = new HashMap<>();
        trendParams.put("output", List.of("clock", "value_avg"));
        trendParams.put("itemids", List.of(itemRef.itemId));
        trendParams.put("time_from", timeFrom);
        trendParams.put("time_till", timeTill);
        trendParams.put("sortfield", "clock");
        trendParams.put("sortorder", "ASC");
        Object trendResult = call(config, "trend.get", trendParams, auth);
        List<Map<String, Object>> trendRows = normalizeRows(trendResult);
        if (trendRows.isEmpty()) {
            return List.of();
        }
        for (Map<String, Object> row : trendRows) {
            Object avg = row.get("value_avg");
            if (avg != null) {
                row.put("value", avg);
            }
        }
        return trendRows;
    }

    private List<Map<String, Object>> normalizeRows(Object result) {
        if (!(result instanceof List<?> list)) {
            return List.of();
        }
        List<Map<String, Object>> rows = new ArrayList<>();
        for (Object item : list) {
            if (!(item instanceof Map<?, ?> map)) {
                continue;
            }
            rows.add(normalizeMap(map));
        }
        return rows;
    }

    private Map<String, Object> normalizeMap(Map<?, ?> map) {
        Map<String, Object> normalized = new HashMap<>();
        for (Map.Entry<?, ?> entry : map.entrySet()) {
            if (entry.getKey() != null) {
                normalized.put(String.valueOf(entry.getKey()), entry.getValue());
            }
        }
        return normalized;
    }

    private boolean isNeedMetric(String key) {
        for (String prefix : IF_METRIC_PREFIXES) {
            if (key.startsWith(prefix)) {
                return true;
            }
        }
        return false;
    }

    private boolean hasAnyTag(Object tagsObj) {
        if (!(tagsObj instanceof List<?> tagList) || tagList.isEmpty()) {
            return false;
        }
        for (Object tag : tagList) {
            if (!(tag instanceof Map<?, ?> map)) {
                continue;
            }
            Object tagName = map.get("tag");
            Object tagValue = map.get("value");
            if (tagName == null && tagValue == null) {
                continue;
            }
            String value = tagValue == null ? "" : String.valueOf(tagValue).trim();
            if (StrUtil.isNotBlank(value)) {
                return true;
            }
        }
        return false;
    }

    private String toMetricKey(String key) {
        if (key.startsWith("net.if.out.errors[")) {
            return "out.errors";
        }
        if (key.startsWith("net.if.in.errors[")) {
            return "in.errors";
        }
        if (key.startsWith("net.if.out.discards[")) {
            return "out.discards";
        }
        if (key.startsWith("net.if.in.discards[")) {
            return "in.discards";
        }
        if (key.startsWith("net.if.in[")) {
            return "in";
        }
        if (key.startsWith("net.if.out[")) {
            return "out";
        }
        return null;
    }

    private String extractInterfaceIndex(String key) {
        if (StrUtil.isBlank(key)) {
            return null;
        }
        int left = key.indexOf('[');
        int right = key.indexOf(']');
        if (left < 0 || right <= left) {
            return null;
        }
        String inside = key.substring(left + 1, right).trim();
        if (StrUtil.isBlank(inside)) {
            return null;
        }
        String first = inside.split(",")[0].trim();
        return normalizeInterfaceIdentifier(first);
    }

    private String normalizeInterfaceIdentifier(String raw) {
        String text = StrUtil.blankToDefault(raw, "").trim();
        if (StrUtil.isBlank(text)) {
            return null;
        }
        if ((text.startsWith("\"") && text.endsWith("\"")) || (text.startsWith("'") && text.endsWith("'"))) {
            text = text.substring(1, text.length() - 1).trim();
        }
        int dot = text.lastIndexOf('.');
        if (dot > 0 && dot < text.length() - 1) {
            String suffix = text.substring(dot + 1).trim();
            if (suffix.matches("^\\d+$")) {
                return suffix;
            }
        }
        return text;
    }

    private int parseInt(Object value, int def) {
        if (value == null) {
            return def;
        }
        try {
            return Integer.parseInt(String.valueOf(value));
        } catch (Exception ignored) {
            return def;
        }
    }

    private static class ItemRef {
        private String itemId;
        private int valueType;
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

    private String stripVersionPrefix(String version) {
        if (version == null) {
            return null;
        }
        String v = version.trim();
        if (v.startsWith("v") || v.startsWith("V")) {
            v = v.substring(1);
        }
        return v;
    }

    private String normalizeVersion(String version) {
        String v = stripVersionPrefix(version);
        if (v == null) {
            return "";
        }
        int plus = v.indexOf('+');
        if (plus > 0) {
            v = v.substring(0, plus);
        }
        int dash = v.indexOf('-');
        if (dash > 0) {
            v = v.substring(0, dash);
        }
        return v;
    }

    private int parseVersionInt(String value) {
        if (StrUtil.isBlank(value)) {
            return 0;
        }
        try {
            Matcher matcher = Pattern.compile("(\\d+)").matcher(value);
            return matcher.find() ? Integer.parseInt(matcher.group(1)) : 0;
        } catch (Exception e) {
            return 0;
        }
    }
}
