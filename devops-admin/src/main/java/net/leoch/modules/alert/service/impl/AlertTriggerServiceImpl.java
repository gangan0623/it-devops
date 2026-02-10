package net.leoch.modules.alert.service.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import lombok.extern.slf4j.Slf4j;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import cn.hutool.core.lang.TypeReference;
import net.leoch.common.data.page.PageData;
import cn.hutool.core.bean.BeanUtil;
import net.leoch.common.data.validator.AssertUtils;
import cn.hutool.json.JSONUtil;
import net.leoch.modules.alert.mapper.AlertMediaMapper;
import net.leoch.modules.alert.service.IAlertNotifyLogService;
import net.leoch.modules.alert.mapper.AlertRecordMapper;
import net.leoch.modules.alert.mapper.AlertTemplateMapper;
import net.leoch.modules.alert.mapper.AlertTriggerMapper;
import net.leoch.modules.alert.vo.req.AlertTriggerPageReq;
import net.leoch.modules.alert.vo.req.AlertTriggerReq;
import net.leoch.modules.alert.vo.rsp.AlertTriggerRsp;
import net.leoch.modules.alert.entity.AlertMediaEntity;
import net.leoch.modules.alert.entity.AlertNotifyLogEntity;
import net.leoch.modules.alert.entity.AlertRecordEntity;
import net.leoch.modules.alert.entity.AlertTemplateEntity;
import net.leoch.modules.alert.entity.AlertTriggerEntity;
import net.leoch.modules.alert.service.AlertMailService;
import net.leoch.modules.alert.service.IAlertTriggerService;
import net.leoch.common.utils.alert.AlertJsonUtils;
import net.leoch.common.utils.alert.AlertPayloadUtils;
import net.leoch.common.utils.alert.AlertTemplateRenderer;
import net.leoch.modules.sys.mapper.SysUserMapper;
import net.leoch.modules.sys.entity.SysUserEntity;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
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
@Slf4j
@Service
public class AlertTriggerServiceImpl extends ServiceImpl<AlertTriggerMapper, AlertTriggerEntity> implements IAlertTriggerService {

    private final AlertTemplateMapper alertTemplateMapper;
    private final AlertMediaMapper alertMediaMapper;
    private final SysUserMapper sysUserMapper;
    private final AlertMailService alertMailService;
    private final IAlertNotifyLogService alertNotifyLogService;
    private final AlertRecordMapper alertRecordMapper;

    public AlertTriggerServiceImpl(AlertTemplateMapper alertTemplateMapper,
                                   AlertMediaMapper alertMediaMapper,
                                   SysUserMapper sysUserMapper,
                                   AlertMailService alertMailService,
                                   IAlertNotifyLogService alertNotifyLogService,
                                   AlertRecordMapper alertRecordMapper) {
        this.alertTemplateMapper = alertTemplateMapper;
        this.alertMediaMapper = alertMediaMapper;
        this.sysUserMapper = sysUserMapper;
        this.alertMailService = alertMailService;
        this.alertNotifyLogService = alertNotifyLogService;
        this.alertRecordMapper = alertRecordMapper;
    }

    @Override
    public PageData<AlertTriggerRsp> page(AlertTriggerPageReq request) {
        IPage<AlertTriggerEntity> page = this.page(request.buildPage(),
            new LambdaQueryWrapper<AlertTriggerEntity>()
                .like(StrUtil.isNotBlank(request.getName()), AlertTriggerEntity::getName, request.getName())
        );
        return new PageData<>(BeanUtil.copyToList(page.getRecords(), AlertTriggerRsp.class), page.getTotal());
    }

    @Override
    public List<AlertTriggerRsp> list(AlertTriggerPageReq request) {
        List<AlertTriggerEntity> entityList = this.list(
            new LambdaQueryWrapper<AlertTriggerEntity>()
                .like(request != null && StrUtil.isNotBlank(request.getName()), AlertTriggerEntity::getName, request != null ? request.getName() : null)
        );
        return BeanUtil.copyToList(entityList, AlertTriggerRsp.class);
    }

    @Override
    public AlertTriggerRsp get(Long id) {
        return BeanUtil.copyProperties(this.getById(id), AlertTriggerRsp.class);
    }

    @Override
    public PageData<AlertTriggerRsp> pageWithReceivers(AlertTriggerPageReq request) {
        PageData<AlertTriggerRsp> pageData = this.page(request);
        fillReceiverUserIdList(pageData.getList());
        return pageData;
    }

    @Override
    public AlertTriggerRsp getWithReceivers(Long id) {
        AlertTriggerRsp dto = this.get(id);
        fillReceiverUserIdList(dto);
        return dto;
    }

    @Override
    public List<Map<String, Object>> options() {
        List<AlertTriggerEntity> list = this.list();
        return list.stream().map(item -> {
            Map<String, Object> map = new HashMap<>();
            map.put("id", item.getId());
            map.put("name", item.getName());
            return map;
        }).collect(Collectors.toList());
    }

    @Override
    public void save(AlertTriggerReq dto) {
        normalizeReceiverIds(dto);
        AlertTriggerEntity entity = BeanUtil.copyProperties(dto, AlertTriggerEntity.class);
        this.save(entity);
        BeanUtil.copyProperties(entity, dto);
    }

    @Override
    public void update(AlertTriggerReq dto) {
        normalizeReceiverIds(dto);
        this.updateById(BeanUtil.copyProperties(dto, AlertTriggerEntity.class));
    }

    @Override
    public void delete(Long[] ids) {
        AssertUtils.isArrayEmpty(ids, "id");
        this.removeByIds(Arrays.asList(ids));
    }

    @Override
    public Map<String, Object> resources() {
        Map<String, Object> result = new HashMap<>();
        List<AlertTemplateEntity> templates = alertTemplateMapper.selectList(null);
        List<AlertMediaEntity> medias = alertMediaMapper.selectList(null);
        List<SysUserEntity> users = sysUserMapper.selectList(
            new QueryWrapper<SysUserEntity>()
                .select("id", "username", "email")
                .isNotNull("email")
        );
        result.put("templates", templates.stream().map(item -> {
            Map<String, Object> map = new HashMap<>();
            map.put("id", item.getId());
            map.put("name", item.getName());
            return map;
        }).collect(Collectors.toList()));
        result.put("medias", medias.stream().map(item -> {
            Map<String, Object> map = new HashMap<>();
            map.put("id", item.getId());
            map.put("name", item.getName());
            return map;
        }).collect(Collectors.toList()));
        result.put("users", users.stream().map(item -> {
            Map<String, Object> map = new HashMap<>();
            map.put("id", item.getId());
            map.put("name", item.getUsername());
            map.put("email", item.getEmail());
            return map;
        }).collect(Collectors.toList()));
        return result;
    }

    @Override
    public void fillReceiverUserIdList(AlertTriggerRsp dto) {
        if (dto == null) {
            return;
        }
        dto.setReceiverUserIdList(splitIds(dto.getReceiverUserIds()));
    }

    @Override
    public void fillReceiverUserIdList(List<AlertTriggerRsp> list) {
        if (list == null) {
            return;
        }
        for (AlertTriggerRsp dto : list) {
            fillReceiverUserIdList(dto);
        }
    }

    @Override
    public void notifyFromWebhook(Map<String, Object> payload, String rawJson, String severity) {
        if (payload == null) {
            return;
        }
        List<AlertTriggerEntity> triggers = this.list(new LambdaQueryWrapper<AlertTriggerEntity>().eq(AlertTriggerEntity::getStatus, 1));
        if (CollUtil.isEmpty(triggers)) {
            return;
        }
        List<Map<String, Object>> alerts = getAlerts(payload);
        if (CollUtil.isEmpty(alerts)) {
            return;
        }
        for (Map<String, Object> alert : alerts) {
            String severityForAlert = resolveSeverity(payload, alert, severity);
            Long recordId = findRecordId(payload, alert);
            for (AlertTriggerEntity trigger : triggers) {
                if (!matches(trigger, payload, alert, severityForAlert)) {
                    continue;
                }
                sendAlert(trigger, payload, alert, severityForAlert, recordId);
            }
        }
    }

    @Override
    public void sendTest(Long templateId, Long triggerId, String rawJson) {
        AlertTriggerEntity trigger = triggerId == null ? null : this.getById(triggerId);
        if (trigger == null) {
            return;
        }
        if (trigger.getStatus() != null && trigger.getStatus() == 0) {
            return;
        }
        AlertTemplateEntity template = templateId == null ? null : alertTemplateMapper.selectById(templateId);
        if (template == null) {
            return;
        }
        AlertMediaEntity media = trigger.getMediaId() == null ? null : alertMediaMapper.selectById(trigger.getMediaId());
        if (media == null) {
            return;
        }
        Map<String, Object> payload = AlertJsonUtils.parsePayload(rawJson);
        Map<String, Object> alert = CollUtil.isNotEmpty(getAlerts(payload)) ? getAlerts(payload).get(0) : new HashMap<>();
        Map<String, Object> context = buildContext(payload, alert, null, null);
        sendWithTemplate(template, media, trigger.getReceiverUserIds(), context);
    }

    private void sendAlert(AlertTriggerEntity trigger, Map<String, Object> payload, Map<String, Object> alert, String severity, Long recordId) {
        AlertTemplateEntity template = trigger.getTemplateId() == null ? null : alertTemplateMapper.selectById(trigger.getTemplateId());
        AlertMediaEntity media = trigger.getMediaId() == null ? null : alertMediaMapper.selectById(trigger.getMediaId());
        if (template == null || media == null) {
            return;
        }
        if (template.getStatus() != null && template.getStatus() == 0) {
            return;
        }
        if (media.getStatus() != null && media.getStatus() == 0) {
            return;
        }
        Map<String, Object> context = buildContext(payload, alert, severity, recordId);
        sendWithTemplate(template, media, trigger.getReceiverUserIds(), context);
    }

    private void sendWithTemplate(AlertTemplateEntity template, AlertMediaEntity media, String receiverUserIds, Map<String, Object> context) {
        List<String> receivers = getReceiverEmails(receiverUserIds);
        if (CollUtil.isEmpty(receivers)) {
            return;
        }
        String subject = AlertTemplateRenderer.render(template.getEmailSubject(), context);
        String html = AlertTemplateRenderer.render(template.getEmailHtml(), context);
        AlertNotifyLogEntity log = new AlertNotifyLogEntity();
        log.setRecordId(context == null ? null : toLong(context.get("recordId")));
        log.setAlertName(context == null ? null : toStr(context.get("alertname")));
        log.setInstance(context == null ? null : toStr(context.get("instance")));
        log.setSeverity(context == null ? null : toStr(context.get("severity")));
        log.setMediaName(media.getName());
        log.setReceivers(String.join(",", receivers));
        log.setSendTime(new Date());
        try {
            alertMailService.send(media, receivers, subject, null, html);
            log.setSendStatus(1);
        } catch (Exception e) {
            log.setSendStatus(0);
            log.setErrorMessage(StrUtil.sub(e.getMessage(), 0, 500));
        } finally {
            alertNotifyLogService.save(log);
        }
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

    private Map<String, Object> buildContext(Map<String, Object> payload, Map<String, Object> alert, String severityFromPath, Long recordId) {
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
        context.put("duration", formatDuration(parseDate(toStr(alert.get("startsAt"))), parseDate(toStr(alert.get("endsAt")))));
        context.put("recordId", recordId);
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
            return JSONUtil.toBean(matchLabels, new TypeReference<Map<String, Object>>() {});
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

    private Long findRecordId(Map<String, Object> payload, Map<String, Object> alert) {
        Map<String, Object> labels = toMap(alert == null ? null : alert.get("labels"));
        String alertName = getLabelValue(labels, toMap(payload.get("commonLabels")), "alertname");
        String instance = getLabelValue(labels, toMap(payload.get("commonLabels")), "instance", getLabelValue(labels, toMap(payload.get("commonLabels")), "service"));
        String startsAt = alert == null ? null : String.valueOf(alert.get("startsAt"));
        Date startsAtDate = parseDate(startsAt);
        AlertRecordEntity record = alertRecordMapper.selectOne(
            new QueryWrapper<AlertRecordEntity>()
                .eq(StrUtil.isNotBlank(alertName), "alert_name", alertName)
                .eq(StrUtil.isNotBlank(instance), "instance", instance)
                .eq(startsAtDate != null, "starts_at", startsAtDate)
                .orderByDesc("create_date")
                .last("limit 1")
        );
        if (record != null) {
            return record.getId();
        }
        AlertRecordEntity fallback = alertRecordMapper.selectOne(
            new QueryWrapper<AlertRecordEntity>()
                .eq(StrUtil.isNotBlank(alertName), "alert_name", alertName)
                .eq(StrUtil.isNotBlank(instance), "instance", instance)
                .orderByDesc("create_date")
                .last("limit 1")
        );
        return fallback == null ? null : fallback.getId();
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

    private void normalizeReceiverIds(AlertTriggerReq dto) {
        if (dto == null || CollUtil.isEmpty(dto.getReceiverUserIdList())) {
            return;
        }
        dto.setReceiverUserIds(dto.getReceiverUserIdList().stream()
            .map(String::valueOf)
            .collect(Collectors.joining(",")));
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
        List<SysUserEntity> users = sysUserMapper.selectList(
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

    private Date parseDate(String value) {
        if (StrUtil.isBlank(value)) {
            return null;
        }
        try {
            return Date.from(Instant.parse(value));
        } catch (Exception ignore) {
            return null;
        }
    }

    private String formatDuration(Date startsAt, Date endsAt) {
        if (startsAt == null) {
            return "-";
        }
        long end = endsAt == null ? System.currentTimeMillis() : endsAt.getTime();
        long seconds = Math.max(0, (end - startsAt.getTime()) / 1000);
        long days = seconds / 86400;
        long hours = (seconds % 86400) / 3600;
        long minutes = (seconds % 3600) / 60;
        if (days > 0) {
            return days + "天" + hours + "小时";
        }
        if (hours > 0) {
            return hours + "小时" + minutes + "分钟";
        }
        return minutes + "分钟";
    }

    private Long toLong(Object value) {
        if (value == null) {
            return null;
        }
        try {
            return Long.parseLong(String.valueOf(value));
        } catch (Exception ignore) {
            return null;
        }
    }

    private String toStr(Object value) {
        return value == null ? null : String.valueOf(value);
    }
}
