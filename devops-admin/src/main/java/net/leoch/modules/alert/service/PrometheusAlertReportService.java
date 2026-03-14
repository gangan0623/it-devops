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
import net.leoch.modules.alert.entity.AlertRecordEntity;
import net.leoch.modules.alert.entity.PrometheusAlertAiReportEntity;
import net.leoch.modules.alert.mapper.AlertRecordMapper;
import net.leoch.modules.alert.mapper.PrometheusAlertAiReportMapper;
import net.leoch.modules.alert.vo.rsp.PrometheusAlertAiReportRsp;
import net.leoch.modules.sys.service.AiConfigService;
import net.leoch.modules.sys.vo.rsp.SysAiConfigRsp;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
@Service
@RequiredArgsConstructor
public class PrometheusAlertReportService {

    private static final int DEFAULT_MAX_TOKENS = 65535;

    private static final List<String> SERVER_GROUPS = List.of("linux_exporter", "windows_exporter");
    private static final List<String> HTTP_GROUPS   = List.of("blackbox_exporter");

    private static final String SYSTEM_PROMPT =
            "你是一名企业级运维分析专家。你必须严格返回JSON对象，不要返回Markdown，不要返回解释文本。";

    private static final String SERVER_USER_PROMPT = """
            你将收到一个监控周期内服务器告警（Linux/Windows）的聚合统计JSON，请严格按以下格式返回JSON对象（字段名不能改，不能缺失）：
            {
              "executive_summary": ["...","..."],
              "risk_top5": [
                {"host": "", "issue": "宕机|CPU|内存|磁盘|其他", "count": 0, "reason": ""}
              ],
              "fault_analysis": ["...","..."],
              "optimization_p1": ["...","..."],
              "optimization_p2": ["...","..."],
              "optimization_p3": ["...","..."],
              "next_actions": ["...","..."]
            }
            规则：
            1) 只返回JSON，不要代码块标记，不要多余文本。
            2) risk_top5 最多5条，按风险高到低排序。
            3) 若无数据，数组给出"本周期暂无有效告警数据"。
            4) 所有建议必须可执行、可落地。
            5) 围绕服务器（Linux/Windows）给出分析，禁止输出网络设备类建议。
            6) {{periodGuidance}}

            周期开始：{{periodStart}}
            周期结束：{{periodEnd}}
            聚合数据：
            {{statsJson}}
            """;

    private static final String HTTP_USER_PROMPT = """
            你将收到一个监控周期内HTTP探测告警（blackbox_exporter）的聚合统计JSON，请严格按以下格式返回JSON对象（字段名不能改，不能缺失）：
            {
              "executive_summary": ["...","..."],
              "risk_top5": [
                {"target": "", "issue": "探测失败|响应慢|SSL证书", "count": 0, "reason": ""}
              ],
              "fault_analysis": ["...","..."],
              "optimization_p1": ["...","..."],
              "optimization_p2": ["...","..."],
              "optimization_p3": ["...","..."],
              "next_actions": ["...","..."]
            }
            规则：
            1) 只返回JSON，不要代码块标记，不要多余文本。
            2) risk_top5 最多5条，按风险高到低排序。
            3) 若无数据，数组给出"本周期暂无有效告警数据"。
            4) 所有建议必须可执行、可落地。
            5) 围绕HTTP探测可用性、响应时间、SSL证书给出分析。
            6) {{periodGuidance}}

            周期开始：{{periodStart}}
            周期结束：{{periodEnd}}
            聚合数据：
            {{statsJson}}
            """;

    private final AlertRecordMapper alertRecordMapper;
    private final PrometheusAlertAiReportMapper reportMapper;
    private final AiConfigService aiConfigService;

    public PrometheusAlertAiReportRsp submitGenerate(String reportType, String periodType, Date start, Date end) {
        String resolvedReportType = StrUtil.blankToDefault(reportType, "server");
        String resolvedPeriodType = StrUtil.blankToDefault(periodType, "week");
        Date now = new Date();
        Date periodEnd = end == null ? now : end;
        Date periodStart = start == null ? defaultStart(periodEnd, resolvedPeriodType) : start;

        PrometheusAlertAiReportEntity report = new PrometheusAlertAiReportEntity();
        report.setReportType(resolvedReportType);
        report.setPeriodType(resolvedPeriodType);
        report.setPeriodStart(periodStart);
        report.setPeriodEnd(periodEnd);
        report.setCreateDate(now);
        report.setReportStatus(0);
        report.setSummary("报告生成中，请稍后查看");

        try {
            SysAiConfigRsp ai = aiConfigService.getConfig();
            if (ai != null) {
                report.setModelName(ai.getModel());
            }
        } catch (Exception e) {
            log.warn("[PrometheusAIReport] load ai config failed before submit, reportType={}", resolvedReportType, e);
        }

        reportMapper.insert(report);
        return BeanUtil.copyProperties(report, PrometheusAlertAiReportRsp.class);
    }

    public PrometheusAlertAiReportRsp generate(String reportType, String periodType, Date start, Date end) {
        PrometheusAlertAiReportRsp rsp = submitGenerate(reportType, periodType, start, end);
        processReport(rsp.getId());
        return getById(rsp.getId());
    }

    public void processReport(Long reportId) {
        if (reportId == null) {
            return;
        }
        PrometheusAlertAiReportEntity report = reportMapper.selectById(reportId);
        if (report == null) {
            return;
        }

        try {
            SysAiConfigRsp ai = aiConfigService.getConfig();
            if (ai == null || ai.getStatus() == null || ai.getStatus() != 1) {
                throw new ServiceException("AI配置未启用");
            }

            String reportType = StrUtil.blankToDefault(report.getReportType(), "server");
            String periodType = StrUtil.blankToDefault(report.getPeriodType(), "week");
            Date periodStart = report.getPeriodStart();
            Date periodEnd = report.getPeriodEnd();
            Map<String, Object> stats = buildStats(reportType, periodStart, periodEnd);
            String inputJson = JSONUtil.toJsonStr(stats);

            report.setModelName(ai.getModel());
            report.setInputJson(inputJson);

            JSONObject reportJsonObj = invokeAi(ai, reportType, inputJson, periodStart, periodEnd, periodType);
            report.setReportJson(reportJsonObj.toString());
            report.setReportMarkdown(toMarkdown(reportJsonObj, reportType, periodType));
            report.setSummary(buildSummary(stats, reportType));
            report.setReportStatus(1);
            report.setErrorMessage(null);
        } catch (Exception e) {
            report.setReportStatus(2);
            report.setErrorMessage(e.getMessage());
            report.setSummary("AI报告生成失败");
            log.warn("[PrometheusAIReport] generate failed, reportId={}, reportType={}", reportId, report.getReportType(), e);
        }

        reportMapper.updateById(report);
    }

    public List<PrometheusAlertAiReportRsp> latest(String reportType, int size) {
        int limit = size <= 0 ? 20 : Math.min(size, 200);
        QueryWrapper<PrometheusAlertAiReportEntity> wrapper = new QueryWrapper<PrometheusAlertAiReportEntity>()
                .orderByDesc("id")
                .last("limit " + limit);
        if (StrUtil.isNotBlank(reportType)) {
            wrapper.eq("report_type", reportType);
        }
        return BeanUtil.copyToList(reportMapper.selectList(wrapper), PrometheusAlertAiReportRsp.class);
    }

    public PrometheusAlertAiReportRsp getById(Long id) {
        if (id == null) {
            return null;
        }
        PrometheusAlertAiReportEntity entity = reportMapper.selectById(id);
        return entity == null ? null : BeanUtil.copyProperties(entity, PrometheusAlertAiReportRsp.class);
    }

    private Map<String, Object> buildStats(String reportType, Date start, Date end) {
        boolean isHttp = "http".equalsIgnoreCase(reportType);
        List<String> groups = isHttp ? HTTP_GROUPS : SERVER_GROUPS;

        // 单次查询，Java 单次遍历完成所有聚合，避免 3 次重复扫表
        List<Map<String, Object>> rows = alertRecordMapper.selectMaps(
                new QueryWrapper<AlertRecordEntity>()
                        .in("alert_group", groups)
                        .between("create_date", start, end)
                        .select("alert_name", "instance", "status")
        );

        long total = 0, firingCount = 0, resolvedCount = 0;
        Map<String, Long> alertNameAcc = new HashMap<>();
        Map<String, long[]> instanceAcc = new HashMap<>();

        for (Map<String, Object> row : rows) {
            total++;
            String status = strVal(row, "status");
            boolean isFiring = "firing".equalsIgnoreCase(status);
            if (isFiring) {
                firingCount++;
            } else if ("resolved".equalsIgnoreCase(status)) {
                resolvedCount++;
            }
            alertNameAcc.merge(strVal(row, "alert_name"), 1L, Long::sum);
            long[] inst = instanceAcc.computeIfAbsent(strVal(row, "instance"), k -> new long[2]);
            inst[0]++;
            if (isFiring) inst[1]++;
        }

        Map<String, Object> stats = new HashMap<>();
        stats.put("total", total);
        stats.put("firingCount", firingCount);
        stats.put("resolvedCount", resolvedCount);

        List<Map<String, Object>> alertNameDist = alertNameAcc.entrySet().stream()
                .sorted(Map.Entry.<String, Long>comparingByValue().reversed())
                .limit(20)
                .map(e -> Map.<String, Object>of("alertName", e.getKey(), "count", e.getValue()))
                .toList();
        stats.put("alertNameDist", alertNameDist);

        String instanceKey = isHttp ? "topTargets" : "topHosts";
        List<Map<String, Object>> topInstances = instanceAcc.entrySet().stream()
                .sorted(Comparator.comparingLong((Map.Entry<String, long[]> e) -> e.getValue()[0]).reversed())
                .limit(20)
                .map(e -> Map.<String, Object>of("instance", e.getKey(), "total_count", e.getValue()[0], "firing_count", e.getValue()[1]))
                .toList();
        stats.put(instanceKey, topInstances);

        return stats;
    }

    private static String strVal(Map<String, Object> row, String key) {
        Object v = row.get(key);
        return v == null ? "" : String.valueOf(v);
    }

    private String buildSummary(Map<String, Object> stats, String reportType) {
        long total    = toLong(stats.get("total"));
        long firing   = toLong(stats.get("firingCount"));
        long resolved = toLong(stats.get("resolvedCount"));
        String typeLabel = "http".equalsIgnoreCase(reportType) ? "HTTP探测" : "服务器";
        return typeLabel + "告警总计" + total + "条，告警中" + firing + "条，已恢复" + resolved + "条";
    }

    private long toLong(Object value) {
        if (value == null) return 0L;
        if (value instanceof Number n) return n.longValue();
        try { return Long.parseLong(String.valueOf(value)); } catch (Exception ignore) { return 0L; }
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

    private JSONObject invokeAi(SysAiConfigRsp ai, String reportType, String inputJson,
                                Date start, Date end, String periodType) {
        try {
            String content = aiConfigService.invokeText(
                    ai,
                    SYSTEM_PROMPT,
                    buildPrompt(reportType, inputJson, start, end, periodType),
                    DEFAULT_MAX_TOKENS
            );
            JSONObject parsed = parseJsonContent(content);
            if (parsed == null) {
                throw new ServiceException("AI返回非JSON内容，已拒绝入库");
            }
            return parsed;
        } catch (ServiceException e) {
            throw e;
        } catch (Exception e) {
            throw new ServiceException("AI接口调用异常: " + e.getMessage());
        }
    }

    private String buildPrompt(String reportType, String inputJson, Date start, Date end, String periodType) {
        String template = "http".equalsIgnoreCase(reportType) ? HTTP_USER_PROMPT : SERVER_USER_PROMPT;
        String periodGuidance = "month".equalsIgnoreCase(periodType)
                ? "本次为【月报】分析，next_actions 请给出下月内可落地的月度改善行动，可适当拓展优化深度与广度。"
                : "本次为【周报】分析，next_actions 请给出下周7天内可立即落地的具体行动，聚焦高频告警和紧急处置。";
        return template
                .replace("{{periodGuidance}}", periodGuidance)
                .replace("{{periodStart}}", String.valueOf(start))
                .replace("{{periodEnd}}", String.valueOf(end))
                .replace("{{statsJson}}", inputJson);
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
        int s = text.indexOf('{');
        int e = text.lastIndexOf('}');
        if (s >= 0 && e > s) {
            try {
                return JSONUtil.parseObj(text.substring(s, e + 1));
            } catch (Exception ignore) {
                // ignore
            }
        }
        return null;
    }

    private String toMarkdown(JSONObject root, String reportType, String periodType) {
        List<String> summary = toLines(root.get("executive_summary"));
        List<String> analysis = toLines(root.get("fault_analysis"));
        List<String> p1 = toLines(root.get("optimization_p1"));
        List<String> p2 = toLines(root.get("optimization_p2"));
        List<String> p3 = toLines(root.get("optimization_p3"));
        List<String> actions = toLines(root.get("next_actions"));
        String actionTitle = "month".equalsIgnoreCase(periodType) ? "下月行动清单" : "下周行动清单";

        StringBuilder sb = new StringBuilder(2048);
        appendBlock(sb, "##", "执行摘要", summary);
        appendRiskTop5(sb, root.get("risk_top5"), reportType);
        appendBlock(sb, "##", "故障结构分析", analysis);
        sb.append("## 优化建议（P1/P2/P3）\n");
        appendBlock(sb, "###", "P1", p1);
        appendBlock(sb, "###", "P2", p2);
        appendBlock(sb, "###", "P3", p3);
        appendBlock(sb, "##", actionTitle, actions);
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

    private void appendBlock(StringBuilder sb, String marker, String title, List<String> lines) {
        sb.append(marker).append(" ").append(title).append("\n");
        for (String line : lines) {
            sb.append("- ").append(line).append("\n");
        }
        sb.append("\n");
    }

    private void appendRiskTop5(StringBuilder sb, Object obj, String reportType) {
        sb.append("## 风险TOP5\n");
        if (!(obj instanceof JSONArray arr) || arr.isEmpty()) {
            sb.append("- 本周期暂无有效告警数据\n\n");
            return;
        }
        boolean isHttp = "http".equalsIgnoreCase(reportType);
        int idx = 1;
        for (Object item : arr) {
            if (!(item instanceof JSONObject row)) {
                continue;
            }
            String hostOrTarget = isHttp
                    ? StrUtil.nullToDefault(row.getStr("target"), "-")
                    : StrUtil.nullToDefault(row.getStr("host"), "-");
            sb.append(idx++).append(". ").append(hostOrTarget)
                    .append("，问题：").append(StrUtil.nullToDefault(row.getStr("issue"), "其他"))
                    .append("，次数：").append(row.getInt("count", 0))
                    .append("，说明：").append(StrUtil.nullToDefault(row.getStr("reason"), "-"))
                    .append("\n");
            if (idx > 5) {
                break;
            }
        }
        sb.append("\n");
    }

}
