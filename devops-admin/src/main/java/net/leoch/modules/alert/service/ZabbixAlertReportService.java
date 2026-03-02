package net.leoch.modules.alert.service;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONArray;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.exception.ServiceException;
import net.leoch.framework.config.ops.HttpTimeoutConfig;
import net.leoch.modules.alert.entity.ZabbixAlertAiReportEntity;
import net.leoch.modules.alert.entity.ZabbixAlertEventHistoryEntity;
import net.leoch.modules.alert.mapper.ZabbixAlertAiReportMapper;
import net.leoch.modules.alert.mapper.ZabbixAlertEventHistoryMapper;
import net.leoch.modules.alert.vo.rsp.ZabbixAlertAiReportRsp;
import net.leoch.modules.sys.service.AiConfigService;
import net.leoch.modules.sys.vo.rsp.SysAiConfigRsp;
import org.springframework.stereotype.Service;

import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Slf4j
@Service
@RequiredArgsConstructor
public class ZabbixAlertReportService {
    private static final double DEFAULT_TEMPERATURE = 0.2D;
    private static final int DEFAULT_MAX_TOKENS = 65535;
    private static final int AI_CONNECT_TIMEOUT_MS = 60_000;
    private static final int AI_READ_TIMEOUT_MS = 300_000;
    private static final BiFunction<Object, Object, Object> LONG_ADD = (a, b) -> (Long) a + (Long) b;
    private static final Pattern INTERFACE_PATTERN = Pattern.compile("接口\\s*([^：:]+?)\\s*[：:]");
    private static final Pattern SDWAN_INTERFACE_PATTERN = Pattern.compile("SD-WAN\\s*\\[([^]]+)]\\s*[：:]\\s*\\[([^]]+)]");
    private static final Set<String> LINK_DOWN_PATTERNS = Set.of("链路断开", "link down");
    private static final Set<String> SPEED_DROP_PATTERNS = Set.of("以太网速率已降至低于先前速率");
    private static final Set<String> DUPLEX_PATTERNS = Set.of("处于半双工模式", "半双工");
    private static final Set<String> ERROR_PATTERNS = Set.of("高错误率", "错误率高", "错包");
    private static final Set<String> DROP_PATTERNS = Set.of("丢包率高", "丢包");
    private static final Set<String> AVAILABILITY_PATTERNS = Set.of(
            "icmp ping 不可用", "icmp ping 不通", "无 snmp 数据收集", "无 snmp 数据采集",
            "运行状况检查状态失效", "主机已重新启动", "设备已重新启动"
    );
    private static final Set<String> UTILIZATION_PATTERNS = Set.of("带宽使用率高", "响应时间长", "cpu 利用率高", "内存利用率高");
    private static final Set<String> HARDWARE_PATTERNS = Set.of("温度高于", "风扇处于严重状态", "电源处于严重状态", "可用磁盘空间");
    private static final Set<String> CONFIG_PATTERNS = Set.of("系统名称已更改", "操作系统描述已更改", "设备已更换", "设备已被更换");

    private static final String SYSTEM_PROMPT = "你是一名企业级运维分析专家。你必须严格返回JSON对象，不要返回Markdown，不要返回解释文本。";
    private static final String USER_PROMPT = """
            你将收到一个监控周期的聚合统计JSON，请严格按以下格式返回JSON对象（字段名不能改，不能缺失）：
                        {
                          “executive_summary”: [“...”,”...”,”...”],
                          “risk_top5”: [
                            {“host_name”:””, “host_ip”:””, “fault_point”:”链路|速率|丢包|错包|可用性|其他”, “count”:0, “reason”:””}
                          ],
                          “fault_analysis”: [“...”,”...”],
                          “optimization_p1”: [“...”,”...”],
                          “optimization_p2”: [“...”,”...”],
                          “optimization_p3”: [“...”,”...”],
                          “next_week_actions”: [“...”,”...”,”...”]
                        }
                        规则：
                        1) 只返回JSON，不要代码块标记，不要多余文本。
                        2) risk_top5 必须最多5条，按风险高到低排序。
                        3) 若无数据，数组给出”本周期暂无有效告警数据”。
                        4) 所有建议必须可执行、可落地。
                        5) 当前环境仅有网络设备（华为/华三/飞塔），禁止输出Linux主机、应用服务、数据库类建议。
                        6) 优先围绕接口链路抖动、端口速率下降、丢包/错包、设备可达性异常给出分析。
                        7) 如果聚合数据中包含 topInterfaceRisks，请优先基于”主机+接口”输出高风险点。
                        8) 如果 classificationCoverage.unmatched_count > 0，需要在执行摘要中明确”存在未匹配触发器类型”，并给出补齐建议。
                        9) {{periodGuidance}}
            
                        周期开始：{{periodStart}}
                        周期结束：{{periodEnd}}
                        聚合数据：
                        {{statsJson}}
            """;

    private final ZabbixAlertEventHistoryMapper historyMapper;
    private final ZabbixAlertAiReportMapper reportMapper;
    private final AiConfigService aiConfigService;
    private final HttpTimeoutConfig httpTimeoutConfig;

    public ZabbixAlertAiReportRsp generate(String periodType, Date start, Date end) {
        Date periodEnd = end == null ? new Date() : end;
        Date periodStart = start == null ? defaultStart(periodEnd, periodType) : start;
        String resolvedPeriodType = StrUtil.blankToDefault(periodType, "week");

        Map<String, Object> stats = buildStats(periodStart, periodEnd);
        String inputJson = JSONUtil.toJsonStr(stats);

        ZabbixAlertAiReportEntity report = new ZabbixAlertAiReportEntity();
        report.setPeriodType(resolvedPeriodType);
        report.setPeriodStart(periodStart);
        report.setPeriodEnd(periodEnd);
        report.setInputJson(inputJson);
        report.setCreateDate(new Date());

        try {
            SysAiConfigRsp ai = aiConfigService.getConfig();
            if (ai == null || ai.getStatus() == null || ai.getStatus() != 1) {
                throw new ServiceException("AI配置未启用");
            }
            report.setModelName(ai.getModel());
            JSONObject reportJsonObj = invokeAi(ai, stats, periodStart, periodEnd, resolvedPeriodType);
            String reportJson = reportJsonObj.toString();
            report.setReportJson(reportJson);
            report.setReportMarkdown(toMarkdown(reportJsonObj, resolvedPeriodType));
            report.setSummary(buildSummary(stats));
            report.setReportStatus(1);
        } catch (Exception e) {
            report.setReportStatus(2);
            report.setErrorMessage(e.getMessage());
            report.setSummary("AI报告生成失败");
            log.warn("[ZabbixAIReport] generate failed", e);
        }

        reportMapper.insert(report);
        return BeanUtil.copyProperties(report, ZabbixAlertAiReportRsp.class);
    }

    public List<ZabbixAlertAiReportRsp> latest(int size) {
        int limit = size <= 0 ? 20 : Math.min(size, 200);
        List<ZabbixAlertAiReportEntity> list = reportMapper.selectList(
                new QueryWrapper<ZabbixAlertAiReportEntity>().orderByDesc("id").last("limit " + limit)
        );
        return BeanUtil.copyToList(list, ZabbixAlertAiReportRsp.class);
    }

    public ZabbixAlertAiReportRsp getById(Long id) {
        if (id == null) {
            return null;
        }
        ZabbixAlertAiReportEntity entity = reportMapper.selectById(id);
        return entity == null ? null : BeanUtil.copyProperties(entity, ZabbixAlertAiReportRsp.class);
    }

    private Date defaultStart(Date end, String periodType) {
        Calendar c = Calendar.getInstance();
        c.setTime(end);
        if ("month".equalsIgnoreCase(periodType)) {
            c.add(Calendar.MONTH, -1);
        } else {
            c.add(Calendar.DAY_OF_MONTH, -7);
        }
        return c.getTime();
    }

    private String buildSummary(Map<String, Object> stats) {
        Number total = toNumber(stats.get("total"));
        Number open = toNumber(stats.get("openCount"));
        Number resolved = toNumber(stats.get("resolvedCount"));
        return "总告警" + total + "条，告警中" + open + "条，已恢复" + resolved + "条";
    }

    private Map<String, Object> buildStats(Date start, Date end) {
        Map<String, Object> totalRow = historyMapper.selectMaps(baseWrapper(start, end)
                .select(
                        "count(*) as total",
                        "sum(case when status='open' then 1 else 0 end) as openCount",
                        "sum(case when status='resolved' then 1 else 0 end) as resolvedCount",
                        "sum(case when severity_code='5' then 1 else 0 end) as disasterCount",
                        "sum(case when severity_code='4' then 1 else 0 end) as highCount",
                        "sum(case when severity_code='3' then 1 else 0 end) as averageCount",
                        "sum(case when severity_code='2' then 1 else 0 end) as warningCount",
                        "sum(case when severity_code='1' then 1 else 0 end) as infoCount"
                )).stream().findFirst().orElseGet(HashMap::new);
        Map<String, Object> stats = new HashMap<>(totalRow);

        // 查询周期内全部事件（含已恢复），按 id 倒序后按 event_id 去重保留最新一条
        List<Map<String, Object>> allRowsRaw = historyMapper.selectMaps(baseWrapper(start, end)
                .select("event_id", "host_ip", "hostname", "trigger_name", "severity_code")
                .orderByDesc("id"));
        List<Map<String, Object>> allRows = deduplicateByEventId(allRowsRaw);

        mergeDetailedStats(stats, allRows);

        List<Map<String, Object>> dailyTrend = historyMapper.selectMaps(baseWrapper(start, end)
                .select("date(event_time) as day", "count(*) as count")
                .groupBy("date(event_time)")
                .orderByAsc("day"));
        stats.put("dailyTrend", dailyTrend);

        return stats;
    }

    private List<Map<String, Object>> deduplicateByEventId(List<Map<String, Object>> rows) {
        Map<String, Map<String, Object>> dedup = new LinkedHashMap<>();
        for (Map<String, Object> row : rows) {
            Object eventIdObj = row.get("event_id");
            if (eventIdObj == null) {
                continue;
            }
            String eventId = String.valueOf(eventIdObj);
            if (StrUtil.isBlank(eventId) || dedup.containsKey(eventId)) {
                continue;
            }
            dedup.put(eventId, row);
        }
        return new ArrayList<>(dedup.values());
    }

    /**
     * 单次遍历完成所有详细统计，替代原来 8 个独立 build 方法。
     */
    private void mergeDetailedStats(Map<String, Object> stats, List<Map<String, Object>> rows) {
        Map<String, Map<String, Object>> hostAcc     = new HashMap<>();
        Map<String, Long>               triggerAcc   = new HashMap<>();
        Map<String, Map<String, Object>> unstableAcc = new HashMap<>();
        Map<String, Map<String, Object>> ifaceAcc    = new HashMap<>();

        long linkDownCount = 0, speedDropCount = 0, duplexCount = 0,
             errorCount = 0, dropCount = 0, availabilityCount = 0;
        long utilizationCount = 0, hardwareCount = 0, configChangeCount = 0, otherExtCount = 0;
        long matchedCount = 0, unmatchedCount = 0;

        for (Map<String, Object> row : rows) {
            String hostIp       = str(row, "host_ip", "-");
            String hostname     = str(row, "hostname", "-");
            String triggerName  = str(row, "trigger_name", "");
            String severityCode = str(row, "severity_code", "");
            String lowerTrigger = triggerName.toLowerCase();
            String faultType    = classifyFaultType(lowerTrigger);

            // ── topHosts ──
            hostAcc.computeIfAbsent(hostIp, k -> {
                Map<String, Object> m = new HashMap<>();
                m.put("host_ip", hostIp); m.put("hostname", hostname); m.put("count", 0L);
                return m;
            }).merge("count", 1L, LONG_ADD);

            // ── topTriggers ──
            triggerAcc.merge(triggerName, 1L, Long::sum);

            // ── faultTypeDistribution ──
            switch (faultType) {
                case "链路"   -> linkDownCount++;
                case "速率"   -> speedDropCount++;
                case "半双工" -> duplexCount++;
                case "错包"   -> errorCount++;
                case "丢包"   -> dropCount++;
                case "可用性" -> availabilityCount++;
            }

            // ── extendedTypeDistribution + classificationCoverage（合并推导，避免重复匹配） ──
            boolean extMatched;
            if      (containsAny(lowerTrigger, UTILIZATION_PATTERNS)) { utilizationCount++;  extMatched = true; }
            else if (containsAny(lowerTrigger, HARDWARE_PATTERNS))    { hardwareCount++;     extMatched = true; }
            else if (containsAny(lowerTrigger, CONFIG_PATTERNS))      { configChangeCount++; extMatched = true; }
            else                                                        { otherExtCount++;    extMatched = false; }
            if (!"其他".equals(faultType) || extMatched) matchedCount++; else unmatchedCount++;

            // ── topUnstableHosts（严重/灾难级别） ──
            if ("4".equals(severityCode) || "5".equals(severityCode)) {
                Map<String, Object> u = unstableAcc.computeIfAbsent(hostIp, k -> {
                    Map<String, Object> m = new HashMap<>();
                    m.put("host_ip", hostIp); m.put("hostname", hostname);
                    m.put("critical_open_count", 0L);
                    m.put("link_down_count", 0L); m.put("speed_drop_count", 0L);
                    m.put("drop_count", 0L); m.put("error_count", 0L); m.put("availability_count", 0L);
                    return m;
                });
                u.merge("critical_open_count", 1L, LONG_ADD);
                String uf = switch (faultType) {
                    case "链路"   -> "link_down_count";
                    case "速率"   -> "speed_drop_count";
                    case "丢包"   -> "drop_count";
                    case "错包"   -> "error_count";
                    case "可用性" -> "availability_count";
                    default -> null;
                };
                if (uf != null) u.merge(uf, 1L, LONG_ADD);
            }

            // ── topInterfaceRisks ──
            String iface = extractInterfaceName(triggerName);
            if (StrUtil.isNotBlank(iface)) {
                Map<String, Object> fi = ifaceAcc.computeIfAbsent(hostIp + "|" + iface, k -> {
                    Map<String, Object> m = new HashMap<>();
                    m.put("host_ip", hostIp); m.put("hostname", hostname);
                    m.put("interface_name", iface); m.put("total_count", 0L);
                    m.put("link_down_count", 0L); m.put("speed_drop_count", 0L);
                    m.put("duplex_count", 0L); m.put("error_count", 0L);
                    m.put("drop_count", 0L); m.put("availability_count", 0L);
                    return m;
                });
                fi.merge("total_count", 1L, LONG_ADD);
                String ff = switch (faultType) {
                    case "链路"   -> "link_down_count";
                    case "速率"   -> "speed_drop_count";
                    case "半双工" -> "duplex_count";
                    case "错包"   -> "error_count";
                    case "丢包"   -> "drop_count";
                    case "可用性" -> "availability_count";
                    default -> null;
                };
                if (ff != null) fi.merge(ff, 1L, LONG_ADD);
            }
        }

        // ── 汇总输出 ──
        List<Map<String, Object>> topHosts = new ArrayList<>(hostAcc.values());
        topHosts.sort(Comparator.comparingLong(m -> -(Long) m.get("count")));
        stats.put("topHosts", topHosts.size() > 10 ? topHosts.subList(0, 10) : topHosts);

        List<Map<String, Object>> topTriggers = new ArrayList<>();
        triggerAcc.forEach((k, v) -> { Map<String, Object> m = new HashMap<>(); m.put("trigger_name", k); m.put("count", v); topTriggers.add(m); });
        topTriggers.sort(Comparator.comparingLong(m -> -(Long) m.get("count")));
        stats.put("topTriggers", topTriggers.size() > 10 ? topTriggers.subList(0, 10) : topTriggers);

        Map<String, Object> faultDist = new HashMap<>();
        faultDist.put("link_down_count", linkDownCount); faultDist.put("speed_drop_count", speedDropCount);
        faultDist.put("duplex_count", duplexCount); faultDist.put("error_count", errorCount);
        faultDist.put("drop_count", dropCount); faultDist.put("availability_count", availabilityCount);
        stats.put("faultTypeDistribution", faultDist);

        Map<String, Object> extDist = new HashMap<>();
        extDist.put("utilization_count", utilizationCount); extDist.put("hardware_count", hardwareCount);
        extDist.put("config_change_count", configChangeCount); extDist.put("other_count", otherExtCount);
        stats.put("extendedTypeDistribution", extDist);

        List<Map<String, Object>> unstableList = new ArrayList<>(unstableAcc.values());
        unstableList.sort(Comparator.comparingLong(m -> -(Long) m.get("critical_open_count")));
        stats.put("topUnstableHosts", unstableList.size() > 10 ? unstableList.subList(0, 10) : unstableList);

        List<Map<String, Object>> ifaceList = new ArrayList<>(ifaceAcc.values());
        ifaceList.sort(Comparator.comparingLong(m -> -(Long) m.get("total_count")));
        stats.put("topInterfaceRisks", ifaceList.size() > 20 ? ifaceList.subList(0, 20) : ifaceList);

        long totalCov = matchedCount + unmatchedCount;
        Map<String, Object> coverage = new HashMap<>();
        coverage.put("matched_count", matchedCount); coverage.put("unmatched_count", unmatchedCount);
        coverage.put("total_count", totalCov);
        coverage.put("matched_ratio", totalCov == 0 ? 1.0 : (double) matchedCount / totalCov);
        stats.put("classificationCoverage", coverage);
    }

    private static String str(Map<String, Object> row, String key, String fallback) {
        Object v = row.get(key);
        return v == null ? fallback : String.valueOf(v);
    }

    private String extractInterfaceName(String triggerName) {
        if (StrUtil.isBlank(triggerName)) {
            return null;
        }
        Matcher sdwanMatcher = SDWAN_INTERFACE_PATTERN.matcher(triggerName);
        if (sdwanMatcher.find()) {
            String hName = StrUtil.trim(sdwanMatcher.group(1));
            String ifName = StrUtil.trim(sdwanMatcher.group(2));
            if (StrUtil.isNotBlank(ifName)) {
                return StrUtil.isNotBlank(hName) ? (hName + "/" + ifName) : ifName;
            }
        }
        Matcher matcher = INTERFACE_PATTERN.matcher(triggerName);
        if (!matcher.find()) {
            return null;
        }
        String raw = StrUtil.trim(matcher.group(1));
        if (StrUtil.isBlank(raw)) {
            return null;
        }
        int idx = raw.indexOf('(');
        if (idx > 0) {
            raw = raw.substring(0, idx).trim();
        }
        return raw;
    }

    private String classifyFaultType(String lowerTrigger) {
        if (containsAny(lowerTrigger, LINK_DOWN_PATTERNS)) {
            return "链路";
        }
        if (containsAny(lowerTrigger, SPEED_DROP_PATTERNS)) {
            return "速率";
        }
        if (containsAny(lowerTrigger, DUPLEX_PATTERNS)) {
            return "半双工";
        }
        if (containsAny(lowerTrigger, ERROR_PATTERNS)) {
            return "错包";
        }
        if (containsAny(lowerTrigger, DROP_PATTERNS)) {
            return "丢包";
        }
        if (containsAny(lowerTrigger, AVAILABILITY_PATTERNS)) {
            return "可用性";
        }
        return "其他";
    }

    private boolean containsAny(String text, Set<String> patterns) {
        for (String pattern : patterns) {
            if (text.contains(pattern)) {
                return true;
            }
        }
        return false;
    }

    private QueryWrapper<ZabbixAlertEventHistoryEntity> baseWrapper(Date start, Date end) {
        return new QueryWrapper<ZabbixAlertEventHistoryEntity>().between("event_time", start, end);
    }

    private JSONObject invokeAi(SysAiConfigRsp ai, Map<String, Object> stats, Date start, Date end, String periodType) {
        HttpURLConnection connection = null;
        try {
            String baseUrl = normalizeBaseUrl(ai.getBaseUrl());
            URL url = new URL(baseUrl + "/v1/chat/completions");
            connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("POST");
            connection.setDoOutput(true);
            connection.setConnectTimeout(Math.max(httpTimeoutConfig.getConnectTimeout(), AI_CONNECT_TIMEOUT_MS));
            connection.setReadTimeout(Math.max(httpTimeoutConfig.getReadTimeout(), AI_READ_TIMEOUT_MS));
            connection.setRequestProperty("Authorization", ai.getApiKey().trim());
            connection.setRequestProperty("Content-Type", "application/json");

            JSONObject body = new JSONObject();
            body.set("model", ai.getModel());
            body.set("temperature", DEFAULT_TEMPERATURE);
            body.set("max_tokens", DEFAULT_MAX_TOKENS);
            body.set("response_format", new JSONObject().set("type", "json_object"));
            JSONArray messages = new JSONArray();
            messages.add(new JSONObject().set("role", "system").set("content", SYSTEM_PROMPT));
            messages.add(new JSONObject().set("role", "user").set("content", buildPrompt(stats, start, end, periodType)));
            body.set("messages", messages);

            try (OutputStream os = connection.getOutputStream()) {
                os.write(body.toString().getBytes(StandardCharsets.UTF_8));
            }

            int code = connection.getResponseCode();
            InputStream in = code >= 200 && code < 300 ? connection.getInputStream() : connection.getErrorStream();
            String response = in != null ? new String(in.readAllBytes(), StandardCharsets.UTF_8) : "";
            if (code < 200 || code >= 300) {
                throw new ServiceException("AI接口调用失败, code=" + code + ", body=" + response);
            }

            JSONObject json = JSONUtil.parseObj(response);
            JSONArray choices = json.getJSONArray("choices");
            if (choices == null || choices.isEmpty()) {
                throw new ServiceException("AI返回内容为空");
            }
            JSONObject message = choices.getJSONObject(0).getJSONObject("message");
            String content = message == null ? null : message.getStr("content");
            if (StrUtil.isBlank(content)) {
                throw new ServiceException("AI返回文本为空");
            }
            JSONObject parsed = parseJsonContent(content);
            if (parsed == null) {
                throw new ServiceException("AI返回非JSON内容，已拒绝入库");
            }
            return parsed;
        } catch (ServiceException e) {
            throw e;
        } catch (Exception e) {
            throw new ServiceException("AI接口调用异常: " + e.getMessage());
        } finally {
            if (connection != null) {
                connection.disconnect();
            }
        }
    }

    private String normalizeBaseUrl(String baseUrl) {
        String value = StrUtil.nullToEmpty(baseUrl).trim();
        while (value.endsWith("/")) {
            value = value.substring(0, value.length() - 1);
        }
        return value;
    }

    private String buildPrompt(Map<String, Object> stats, Date start, Date end, String periodType) {
        String periodGuidance = "month".equalsIgnoreCase(periodType)
                ? "本次为【月报】分析，next_week_actions 请给出下月内可落地的月度改善行动，可适当拓展优化深度与广度，覆盖中长期改进方向。"
                : "本次为【周报】分析，next_week_actions 请给出下周7天内可立即落地的具体行动，聚焦高频告警和紧急处置。";
        return USER_PROMPT
                .replace("{{periodGuidance}}", periodGuidance)
                .replace("{{periodStart}}", String.valueOf(start))
                .replace("{{periodEnd}}", String.valueOf(end))
                .replace("{{statsJson}}", JSONUtil.toJsonStr(stats));
    }

    private JSONObject parseJsonContent(String content) {
        String text = StrUtil.trim(content);
        if (StrUtil.isBlank(text)) {
            return null;
        }
        try {
            return JSONUtil.parseObj(text);
        } catch (Exception ignore) {
            // continue
        }

        int start = text.indexOf('{');
        int end = text.lastIndexOf('}');
        if (start >= 0 && end > start) {
            String sub = text.substring(start, end + 1);
            try {
                return JSONUtil.parseObj(sub);
            } catch (Exception ignore) {
                // ignore
            }
        }
        return null;
    }

    private String toMarkdown(JSONObject root, String periodType) {
        List<String> summary = toLines(root.get("executive_summary"));
        List<String> analysis = toLines(root.get("fault_analysis"));
        List<String> p1 = toLines(root.get("optimization_p1"));
        List<String> p2 = toLines(root.get("optimization_p2"));
        List<String> p3 = toLines(root.get("optimization_p3"));
        List<String> actions = toLines(root.get("next_week_actions"));
        String actionTitle = "month".equalsIgnoreCase(periodType) ? "下月行动清单" : "下周行动清单";

        StringBuilder sb = new StringBuilder(2048);
        appendSection(sb, "执行摘要", summary);
        appendRiskTop5(sb, root.get("risk_top5"));
        appendSection(sb, "故障结构分析", analysis);
        sb.append("## 优化建议（P1/P2/P3）\n");
        appendSubSection(sb, "P1", p1);
        appendSubSection(sb, "P2", p2);
        appendSubSection(sb, "P3", p3);
        appendSection(sb, actionTitle, actions);
        return sb.toString().trim();
    }

    private List<String> toLines(Object obj) {
        List<String> lines = new ArrayList<>();
        if (obj instanceof JSONArray arr) {
            for (Object item : arr) {
                String line = StrUtil.trim(item == null ? null : String.valueOf(item));
                if (StrUtil.isNotBlank(line)) {
                    lines.add(line);
                }
            }
        }
        if (lines.isEmpty()) {
            lines.add("本周期暂无有效告警数据");
        }
        return lines;
    }

    private void appendSection(StringBuilder sb, String title, List<String> lines) {
        sb.append("## ").append(title).append("\n");
        for (String line : lines) {
            sb.append("- ").append(line).append("\n");
        }
        sb.append("\n");
    }

    private void appendSubSection(StringBuilder sb, String title, List<String> lines) {
        sb.append("### ").append(title).append("\n");
        for (String line : lines) {
            sb.append("- ").append(line).append("\n");
        }
        sb.append("\n");
    }

    private void appendRiskTop5(StringBuilder sb, Object obj) {
        sb.append("## 风险TOP5\n");
        if (!(obj instanceof JSONArray arr) || arr.isEmpty()) {
            sb.append("- 本周期暂无有效告警数据\n\n");
            return;
        }
        int idx = 1;
        for (Object item : arr) {
            if (!(item instanceof JSONObject row)) {
                continue;
            }
            sb.append(idx++)
                    .append(". ")
                    .append(StrUtil.nullToDefault(row.getStr("host_name"), "-"))
                    .append(" / ")
                    .append(StrUtil.nullToDefault(row.getStr("host_ip"), "-"))
                    .append("，故障点：")
                    .append(StrUtil.nullToDefault(row.getStr("fault_point"), "其他"))
                    .append("，次数：")
                    .append(toNumber(row.get("count")))
                    .append("，说明：")
                    .append(StrUtil.nullToDefault(row.getStr("reason"), "-"))
                    .append("\n");
            if (idx > 5) {
                break;
            }
        }
        sb.append("\n");
    }

    private Number toNumber(Object value) {
        if (value == null) {
            return 0;
        }
        if (value instanceof Number n) {
            return n;
        }
        try {
            return Long.parseLong(String.valueOf(value));
        } catch (Exception ignore) {
            return 0;
        }
    }
}
