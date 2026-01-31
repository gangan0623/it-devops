package net.leoch.modules.alert.service.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.fasterxml.jackson.core.type.TypeReference;
import net.leoch.common.service.impl.CrudServiceImpl;
import net.leoch.common.utils.JsonUtils;
import net.leoch.modules.alert.dao.AlertMediaDao;
import net.leoch.modules.alert.dao.AlertTemplateDao;
import net.leoch.modules.alert.dao.AlertTriggerDao;
import net.leoch.modules.alert.dto.AlertTriggerDTO;
import net.leoch.modules.alert.entity.AlertMediaEntity;
import net.leoch.modules.alert.entity.AlertTemplateEntity;
import net.leoch.modules.alert.entity.AlertTriggerEntity;
import net.leoch.modules.alert.service.AlertMailService;
import net.leoch.modules.alert.service.AlertTriggerService;
import net.leoch.modules.alert.utils.AlertJsonUtils;
import net.leoch.modules.alert.utils.AlertPayloadUtils;
import net.leoch.modules.alert.utils.AlertTemplateRenderer;
import net.leoch.modules.sys.dao.SysUserDao;
import net.leoch.modules.sys.entity.SysUserEntity;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 告警触发器
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Service
public class AlertTriggerServiceImpl extends CrudServiceImpl<AlertTriggerDao, AlertTriggerEntity, AlertTriggerDTO> implements AlertTriggerService {

    private final AlertTemplateDao alertTemplateDao;
    private final AlertMediaDao alertMediaDao;
    private final SysUserDao sysUserDao;
    private final AlertMailService alertMailService;

    public AlertTriggerServiceImpl(AlertTemplateDao alertTemplateDao, AlertMediaDao alertMediaDao, SysUserDao sysUserDao, AlertMailService alertMailService) {
        this.alertTemplateDao = alertTemplateDao;
        this.alertMediaDao = alertMediaDao;
        this.sysUserDao = sysUserDao;
        this.alertMailService = alertMailService;
    }

    @Override
    public QueryWrapper<AlertTriggerEntity> getWrapper(Map<String, Object> params) {
        String name = (String) params.get("name");
        String status = (String) params.get("status");

        QueryWrapper<AlertTriggerEntity> wrapper = new QueryWrapper<>();
        wrapper.like(StrUtil.isNotBlank(name), "name", name);
        wrapper.eq(StrUtil.isNotBlank(status), "status", status);

        return wrapper;
    }

    @Override
    public void fillReceiverUserIdList(AlertTriggerDTO dto) {
        if (dto == null) {
            return;
        }
        dto.setReceiverUserIdList(splitIds(dto.getReceiverUserIds()));
    }

    @Override
    public void fillReceiverUserIdList(List<AlertTriggerDTO> list) {
        if (list == null) {
            return;
        }
        for (AlertTriggerDTO dto : list) {
            fillReceiverUserIdList(dto);
        }
    }

    @Override
    public void notifyFromWebhook(Map<String, Object> payload, String rawJson, String severity) {
        if (payload == null) {
            return;
        }
        List<AlertTriggerEntity> triggers = baseDao.selectList(new QueryWrapper<AlertTriggerEntity>().eq("status", 1));
        if (CollUtil.isEmpty(triggers)) {
            return;
        }
        List<Map<String, Object>> alerts = getAlerts(payload);
        if (CollUtil.isEmpty(alerts)) {
            return;
        }
        for (Map<String, Object> alert : alerts) {
            String severityForAlert = resolveSeverity(payload, alert, severity);
            for (AlertTriggerEntity trigger : triggers) {
                if (!matches(trigger, payload, alert, severityForAlert)) {
                    continue;
                }
                sendAlert(trigger, payload, alert, rawJson, severityForAlert);
            }
        }
    }

    @Override
    public void sendTest(Long templateId, Long triggerId, String rawJson) {
        AlertTriggerEntity trigger = triggerId == null ? null : baseDao.selectById(triggerId);
        if (trigger == null) {
            return;
        }
        AlertTemplateEntity template = templateId == null ? null : alertTemplateDao.selectById(templateId);
        if (template == null) {
            return;
        }
        AlertMediaEntity media = trigger.getMediaId() == null ? null : alertMediaDao.selectById(trigger.getMediaId());
        if (media == null) {
            return;
        }
        Map<String, Object> payload = AlertJsonUtils.parsePayload(rawJson);
        Map<String, Object> alert = CollUtil.isNotEmpty(getAlerts(payload)) ? getAlerts(payload).get(0) : new HashMap<>();
        Map<String, Object> context = buildContext(payload, alert, null);
        sendWithTemplate(template, media, trigger.getReceiverUserIds(), context);
    }

    private void sendAlert(AlertTriggerEntity trigger, Map<String, Object> payload, Map<String, Object> alert, String rawJson, String severity) {
        AlertTemplateEntity template = trigger.getTemplateId() == null ? null : alertTemplateDao.selectById(trigger.getTemplateId());
        AlertMediaEntity media = trigger.getMediaId() == null ? null : alertMediaDao.selectById(trigger.getMediaId());
        if (template == null || media == null) {
            return;
        }
        if (template.getStatus() != null && template.getStatus() == 0) {
            return;
        }
        if (media.getStatus() != null && media.getStatus() == 0) {
            return;
        }
        Map<String, Object> context = buildContext(payload, alert, severity);
        sendWithTemplate(template, media, trigger.getReceiverUserIds(), context);
    }

    private void sendWithTemplate(AlertTemplateEntity template, AlertMediaEntity media, String receiverUserIds, Map<String, Object> context) {
        List<String> receivers = getReceiverEmails(receiverUserIds);
        if (CollUtil.isEmpty(receivers)) {
            return;
        }
        String subject = AlertTemplateRenderer.render(template.getEmailSubject(), context);
        String html = AlertTemplateRenderer.render(template.getEmailHtml(), context);
        alertMailService.send(media, receivers, subject, null, html);
    }

    private boolean matches(AlertTriggerEntity trigger, Map<String, Object> payload, Map<String, Object> alert, String severityFromPath) {
        if (!matchesSeverity(trigger.getSeverity(), severityFromPath)) {
            return false;
        }
        Map<String, Object> matchLabels = parseMatchLabels(trigger.getMatchLabels());
        if (matchLabels.isEmpty()) {
            return true;
        }
        Map<String, Object> labels = toMap(alert.get("labels"));
        Map<String, Object> commonLabels = toMap(payload.get("commonLabels"));
        for (Map.Entry<String, Object> entry : matchLabels.entrySet()) {
            Object expectedValue = entry.getValue();
            String actual = getLabelValue(labels, commonLabels, entry.getKey());
            if (!matchesExpected(expectedValue, actual)) {
                return false;
            }
        }
        return true;
    }

    private Map<String, Object> buildContext(Map<String, Object> payload, Map<String, Object> alert, String severityFromPath) {
        Map<String, Object> context = new HashMap<>();
        context.put("receiver", payload.get("receiver"));
        context.put("status", payload.get("status"));
        context.put("externalURL", payload.get("externalURL"));
        context.put("groupKey", payload.get("groupKey"));
        context.put("commonLabels", payload.get("commonLabels"));
        context.put("commonAnnotations", payload.get("commonAnnotations"));
        context.put("labels", alert.get("labels"));
        context.put("annotations", alert.get("annotations"));
        context.put("alert", alert);
        Map<String, Object> labels = toMap(alert.get("labels"));
        Map<String, Object> annotations = toMap(alert.get("annotations"));
        context.put("alertname", getLabelValue(labels, toMap(payload.get("commonLabels")), "alertname"));
        String severity = StrUtil.isNotBlank(severityFromPath) ? severityFromPath : getLabelValue(labels, toMap(payload.get("commonLabels")), "severity");
        context.put("severity", AlertPayloadUtils.toSeverityZh(severity));
        context.put("instance", getLabelValue(labels, toMap(payload.get("commonLabels")), "instance", getLabelValue(labels, toMap(payload.get("commonLabels")), "service")));
        context.put("summary", getLabelValue(annotations, toMap(payload.get("commonAnnotations")), "summary"));
        context.put("description", getLabelValue(annotations, toMap(payload.get("commonAnnotations")), "description"));
        context.put("startsAt", alert.get("startsAt"));
        context.put("endsAt", alert.get("endsAt"));
        return context;
    }

    private String resolveSeverity(Map<String, Object> payload, Map<String, Object> alert, String fallback) {
        String alertStatus = alert == null ? null : String.valueOf(alert.get("status"));
        if ("resolved".equalsIgnoreCase(alertStatus)) {
            return "recover";
        }
        Map<String, Object> labels = toMap(alert == null ? null : alert.get("labels"));
        String severity = getLabelValue(labels, toMap(payload.get("commonLabels")), "severity");
        if (StrUtil.isNotBlank(severity)) {
            return severity;
        }
        Object status = payload == null ? null : payload.get("status");
        if ("resolved".equalsIgnoreCase(String.valueOf(status))) {
            return "recover";
        }
        return fallback;
    }

    private Map<String, Object> parseMatchLabels(String matchLabels) {
        if (StrUtil.isBlank(matchLabels)) {
            return new HashMap<>();
        }
        try {
            return JsonUtils.parseObject(matchLabels, new TypeReference<Map<String, Object>>() {});
        } catch (Exception ignore) {
            return new HashMap<>();
        }
    }

    private boolean matchesSeverity(String configured, String actual) {
        if (StrUtil.isBlank(configured)) {
            return true;
        }
        if (StrUtil.isBlank(actual)) {
            return false;
        }
        List<String> list = StrUtil.split(configured, ',').stream()
            .map(String::trim)
            .filter(StrUtil::isNotBlank)
            .collect(Collectors.toList());
        return matchesExpected(list, actual);
    }

    private boolean matchesExpected(Object expectedValue, String actual) {
        if (expectedValue == null) {
            return true;
        }
        if (expectedValue instanceof List) {
            @SuppressWarnings("unchecked")
            List<Object> list = (List<Object>) expectedValue;
            return list.stream().anyMatch(item -> StrUtil.equals(String.valueOf(item), actual));
        }
        String expected = String.valueOf(expectedValue);
        if (StrUtil.contains(expected, ",")) {
            return StrUtil.split(expected, ',').stream()
                .map(String::trim)
                .anyMatch(item -> StrUtil.equals(item, actual));
        }
        return StrUtil.equals(expected, actual);
    }

    @SuppressWarnings("unchecked")
    private Map<String, Object> toMap(Object value) {
        return value instanceof Map ? (Map<String, Object>) value : new HashMap<>();
    }

    private List<Map<String, Object>> getAlerts(Map<String, Object> payload) {
        Object alerts = payload.get("alerts");
        if (!(alerts instanceof List)) {
            return new ArrayList<>();
        }
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> list = (List<Map<String, Object>>) alerts;
        return list;
    }

    private String getLabelValue(Map<String, Object> primary, Map<String, Object> fallback, String key) {
        String value = primary != null && primary.containsKey(key) ? String.valueOf(primary.get(key)) : null;
        if (StrUtil.isNotBlank(value)) {
            return value;
        }
        return fallback != null && fallback.containsKey(key) ? String.valueOf(fallback.get(key)) : null;
    }

    private String getLabelValue(Map<String, Object> primary, Map<String, Object> fallback, String key, String extraFallback) {
        String value = getLabelValue(primary, fallback, key);
        if (StrUtil.isNotBlank(value)) {
            return value;
        }
        return extraFallback;
    }

    private List<Long> splitIds(String ids) {
        if (StrUtil.isBlank(ids)) {
            return new ArrayList<>();
        }
        return StrUtil.split(ids, ',').stream()
            .map(String::trim)
            .filter(StrUtil::isNotBlank)
            .map(Long::valueOf)
            .collect(Collectors.toList());
    }

    private List<String> getReceiverEmails(String receiverUserIds) {
        List<Long> ids = splitIds(receiverUserIds);
        if (ids.isEmpty()) {
            return new ArrayList<>();
        }
        List<SysUserEntity> users = sysUserDao.selectList(
            new QueryWrapper<SysUserEntity>()
                .select("id", "email", "username")
                .in("id", ids)
        );
        return users.stream()
            .map(SysUserEntity::getEmail)
            .filter(StrUtil::isNotBlank)
            .distinct()
            .collect(Collectors.toList());
    }
}
