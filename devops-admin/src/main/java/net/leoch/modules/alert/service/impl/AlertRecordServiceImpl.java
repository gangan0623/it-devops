package net.leoch.modules.alert.service.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.data.page.PageData;
import net.leoch.common.data.validator.AssertUtils;
import net.leoch.common.exception.ServiceException;
import net.leoch.common.utils.common.ParseUtils;
import net.leoch.common.utils.common.TimeUtils;
import net.leoch.modules.alert.entity.AlertNotifyLogEntity;
import net.leoch.modules.alert.entity.AlertRecordActionEntity;
import net.leoch.modules.alert.entity.AlertRecordEntity;
import net.leoch.modules.alert.mapper.AlertRecordMapper;
import net.leoch.modules.alert.service.*;
import net.leoch.modules.alert.vo.req.AlertProblemPageReq;
import net.leoch.modules.alert.vo.req.AlertRecordPageReq;
import net.leoch.modules.alert.vo.rsp.AlertProblemRsp;
import net.leoch.modules.alert.vo.rsp.AlertRecordActionRsp;
import net.leoch.modules.alert.vo.rsp.AlertRecordRsp;
import net.leoch.modules.ops.entity.BusinessSystemEntity;
import net.leoch.modules.ops.entity.LinuxHostEntity;
import net.leoch.modules.ops.entity.WindowHostEntity;
import net.leoch.modules.ops.mapper.BusinessSystemMapper;
import net.leoch.modules.ops.mapper.LinuxHostMapper;
import net.leoch.modules.ops.mapper.WindowHostMapper;
import net.leoch.modules.sys.entity.SysUserEntity;
import net.leoch.modules.sys.mapper.SysUserMapper;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 告警记录
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Slf4j
@Service
public class AlertRecordServiceImpl extends ServiceImpl<AlertRecordMapper, AlertRecordEntity> implements IAlertRecordService {

    private final IAlertSseService alertSseService;
    private final IAlertRecordActionService alertRecordActionService;
    private final AlertManagerService alertManagerService;
    private final IAlertTriggerService alertTriggerService;
    private final IAlertNotifyLogService alertNotifyLogService;
    private final SysUserMapper sysUserMapper;
    private final LinuxHostMapper linuxHostMapper;
    private final WindowHostMapper windowHostMapper;
    private final BusinessSystemMapper businessSystemMapper;

    public AlertRecordServiceImpl(IAlertSseService alertSseService,
                                  IAlertRecordActionService alertRecordActionService,
                                  AlertManagerService alertManagerService,
                                  IAlertTriggerService alertTriggerService,
                                  IAlertNotifyLogService alertNotifyLogService,
                                  SysUserMapper sysUserMapper,
                                  LinuxHostMapper linuxHostMapper,
                                  WindowHostMapper windowHostMapper,
                                  BusinessSystemMapper businessSystemMapper) {
        this.alertSseService = alertSseService;
        this.alertRecordActionService = alertRecordActionService;
        this.alertManagerService = alertManagerService;
        this.alertTriggerService = alertTriggerService;
        this.alertNotifyLogService = alertNotifyLogService;
        this.sysUserMapper = sysUserMapper;
        this.linuxHostMapper = linuxHostMapper;
        this.windowHostMapper = windowHostMapper;
        this.businessSystemMapper = businessSystemMapper;
    }

    @Override
    public PageData<AlertRecordRsp> page(AlertRecordPageReq request) {
        String alertName = request.getAlertName();
        String instance = request.getInstance();
        String hostName = request.getHostName();
        String severity = normalizeSeverityForQuery(request.getSeverity());
        String status = request.getStatus();
        String deviceType = request.getDeviceType();

        LambdaQueryWrapper<AlertRecordEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.like(StrUtil.isNotBlank(alertName), AlertRecordEntity::getAlertName, alertName);
        wrapper.like(StrUtil.isNotBlank(instance), AlertRecordEntity::getInstance, instance);
        if (StrUtil.isNotBlank(hostName)) {
            List<String> instancesByName = listInstancesByHostName(hostName, deviceType);
            if (instancesByName.isEmpty()) {
                wrapper.eq(AlertRecordEntity::getId, -1L);
            } else {
                applyInstanceLikeFilter(wrapper, instancesByName);
            }
        }
        wrapper.eq(StrUtil.isNotBlank(severity), AlertRecordEntity::getSeverity, severity);
        wrapper.eq(StrUtil.isNotBlank(status), AlertRecordEntity::getStatus, status);
        if (StrUtil.isNotBlank(deviceType)) {
            List<String> instances = listInstancesByType(deviceType);
            if (instances.isEmpty()) {
                wrapper.eq(AlertRecordEntity::getId, -1L);
            } else {
                applyInstanceLikeFilter(wrapper, instances);
            }
        }
        wrapper.orderByDesc(AlertRecordEntity::getCreateDate);

        IPage<AlertRecordEntity> page = this.page(request.buildPage(), wrapper);
        PageData<AlertRecordRsp> pageData = new PageData<>(BeanUtil.copyToList(page.getRecords(), AlertRecordRsp.class), page.getTotal());

        if (pageData.getList() == null || pageData.getList().isEmpty()) {
            return pageData;
        }
        Map<String, HostInfo> hostMap = loadHostInfoMap();
        for (AlertRecordRsp dto : pageData.getList()) {
            if (dto == null) {
                continue;
            }
            dto.setHostName(resolveHostName(dto.getInstance(), hostMap));
        }
        return pageData;
    }

    @Override
    public AlertRecordRsp get(Long id) {
        return BeanUtil.copyProperties(this.getById(id), AlertRecordRsp.class);
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void delete(Long[] ids) {
        log.info("[AlertRecord] 开始删除, ids={}", Arrays.toString(ids));
        AssertUtils.isArrayEmpty(ids, "id");
        this.removeByIds(Arrays.asList(ids));
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void saveFromWebhook(Map<String, Object> payload, String rawJson, String severityFromPath) {
        if (payload == null) {
            return;
        }
        Object alertsObj = payload.get("alerts");
        if (!(alertsObj instanceof List)) {
            return;
        }
        String receiver = toStr(payload.get("receiver"));
        String status = toStr(payload.get("status"));
        Map<String, Object> commonLabels = toMap(payload.get("commonLabels"));
        Map<String, Object> commonAnnotations = toMap(payload.get("commonAnnotations"));
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> alerts = (List<Map<String, Object>>) alertsObj;
        boolean saved = false;
        for (Map<String, Object> alert : alerts) {
            Map<String, Object> labels = toMap(alert.get("labels"));
            Map<String, Object> annotations = toMap(alert.get("annotations"));
            String alertName = getValue(labels, commonLabels, "alertname");
            String instance = getValue(labels, commonLabels, "instance", getValue(labels, commonLabels, "service"));
            String alertStatus = toStr(alert.get("status"), status);
            if ("firing".equalsIgnoreCase(alertStatus) && isSuppressed(alertName, instance)) {
                continue;
            }
            AlertRecordEntity entity = new AlertRecordEntity();
            entity.setAlertName(alertName);
            entity.setStatus(alertStatus);
            entity.setSeverity(resolveSeverity(payload, alert, severityFromPath));
            entity.setInstance(instance);
            entity.setSummary(getValue(annotations, commonAnnotations, "summary"));
            entity.setDescription(getValue(annotations, commonAnnotations, "description"));
            entity.setStartsAt(TimeUtils.parseDate(ParseUtils.toStr(alert.get("startsAt"))));
            entity.setEndsAt(TimeUtils.parseDate(ParseUtils.toStr(alert.get("endsAt"))));
            entity.setReceiver(receiver);
            entity.setRawJson(rawJson);
            entity.setClosed(0);
            this.getBaseMapper().insert(entity);
            saved = true;
        }
        if (saved) {
            alertSseService.publishRecentAlerts();
        }
    }

    @Override
    public List<AlertRecordActionRsp> history(Long recordId) {
        return alertRecordActionService.listByRecordId(recordId);
    }

    @Override
    public void changeSeverity(Long recordId, String severity, String message) {
        AlertRecordEntity record = requireRecord(recordId);
        String normalized = normalizeSeverity(severity);
        if (StrUtil.isBlank(normalized)) {
            throw new ServiceException("请选择严重性");
        }
        String oldSeverity = record.getSeverity();
        record.setSeverity(normalized);
        this.updateById(record);
        saveAction(recordId, "更改严重性", message,
            "from=" + StrUtil.nullToEmpty(oldSeverity) + ",to=" + normalized);
        alertSseService.publishRecentAlerts();
    }

    @Override
    public void suppress(Long recordId, Integer days, String message) {
        AlertRecordEntity record = requireRecord(recordId);
        if (days == null || days <= 0) {
            throw new ServiceException("抑制天数必须大于0");
        }

        // 幂等性检查：如果当前告警已在有效抑制期内，则跳过
        if (record.getSuppressedUntil() != null && record.getSuppressedUntil().after(new Date())) {
            log.warn("[告警记录] 已在抑制中, recordId={}, suppressedUntil={}, 跳过重复抑制",
                recordId, record.getSuppressedUntil());
            return;
        }

        Date until = Date.from(Instant.now().plusSeconds(days.longValue() * 24 * 3600));
        LambdaUpdateWrapper<AlertRecordEntity> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.eq(AlertRecordEntity::getAlertName, record.getAlertName())
            .eq(AlertRecordEntity::getInstance, record.getInstance())
            .set(AlertRecordEntity::getSuppressedUntil, until);
        this.update(null, updateWrapper);

        String silenceId = alertManagerService.createSilence(record, days, message);
        String details = "days=" + days + ",until=" + until;
        if (StrUtil.isNotBlank(silenceId)) {
            details = details + ",silenceId=" + silenceId;
        }
        saveAction(recordId, "抑制", message, details);
        log.info("[告警记录] 抑制成功, recordId={}, days={}, until={}", recordId, days, until);
        alertSseService.publishRecentAlerts();
    }

    @Override
    public void acknowledge(Long recordId, String message) {
        requireRecord(recordId);
        saveAction(recordId, "确定", message, null);
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void close(Long recordId, String message) {
        AlertRecordEntity record = requireRecord(recordId);

        // 幂等性检查：如果告警已关闭，则跳过
        if (record.getClosed() != null && record.getClosed() == 1) {
            log.warn("[告警记录] 已关闭, recordId={}, 跳过重复关闭", recordId);
            return;
        }

        record.setClosed(1);
        record.setStatus("resolved");
        // 手动关闭时，恢复时间以"点击关闭"的操作时间为准
        record.setEndsAt(new Date());
        this.updateById(record);

        alertManagerService.sendResolvedAlert(record, message);
        sendRecoveryMail(record, message);
        saveAction(recordId, "关闭", message, "manualClose=true");
        log.info("[告警记录] 关闭成功, recordId={}", recordId);
        alertSseService.publishRecentAlerts();
    }

    @Override
    public PageData<AlertProblemRsp> problemPage(AlertProblemPageReq request) {
        int page = parseInt(toStr(request.getPage()), 1);
        int limit = parseInt(toStr(request.getLimit()), 10);

        LambdaQueryWrapper<AlertRecordEntity> wrapper = buildProblemWrapper(request);

        // 添加合理的上限保护，避免加载过多数据到内存（OOM 风险）
        // 注意：由于需要内存过滤，此处使用较大的上限（10000），确保分页结果的完整性
        // TODO: 未来可考虑将更多过滤条件下推到数据库层，彻底改为数据库分页
        wrapper.last("LIMIT 10000");

        List<AlertRecordEntity> records = this.list(wrapper);
        log.debug("[告警记录] 问题分页查询, 数据库加载记录数={}", records.size());

        List<AlertProblemRsp> filtered = enrichAndFilterProblemRecords(records, request);
        log.debug("[告警记录] 问题分页查询, 内存过滤后记录数={}", filtered.size());

        return paginateProblemResults(filtered, page, limit);
    }

    private LambdaQueryWrapper<AlertRecordEntity> buildProblemWrapper(AlertProblemPageReq request) {
        String category = toStr(request.getCategory(), "realtime");
        List<String> severityList = parseSeverityList(toStr(request.getSeverity()));
        String deviceType = toStr(request.getDeviceType());
        String hostName = toStr(request.getHostName());
        String instance = toStr(request.getInstance());
        String statusFilter = toStr(request.getStatusFilter());
        Date startTime = TimeUtils.parseDate(ParseUtils.toStr(request.getStartTime()));
        Date endTime = TimeUtils.parseDate(ParseUtils.toStr(request.getEndTime()));

        LambdaQueryWrapper<AlertRecordEntity> wrapper = new LambdaQueryWrapper<AlertRecordEntity>()
            .in(!severityList.isEmpty(), AlertRecordEntity::getSeverity, severityList)
            .orderByDesc(AlertRecordEntity::getCreateDate);

        if (StrUtil.isNotBlank(hostName)) {
            List<String> instancesByName = listInstancesByHostName(hostName, deviceType);
            if (instancesByName.isEmpty()) {
                wrapper.like(AlertRecordEntity::getInstance, hostName);
            } else {
                applyInstanceLikeFilter(wrapper, instancesByName);
            }
        }
        if (StrUtil.isNotBlank(instance)) {
            wrapper.like(AlertRecordEntity::getInstance, instance);
        }

        if ("realtime".equalsIgnoreCase(category)) {
            applyRealtimeFilters(wrapper, statusFilter);
        } else {
            applyHistoryFilters(wrapper, startTime, endTime);
        }
        return wrapper;
    }

    private void applyRealtimeFilters(LambdaQueryWrapper<AlertRecordEntity> wrapper, String statusFilter) {
        Date recent12h = Date.from(Instant.now().minusSeconds(12L * 3600));
        wrapper.ge(AlertRecordEntity::getStartsAt, recent12h);
        if ("problem".equalsIgnoreCase(statusFilter)) {
            wrapper.eq(AlertRecordEntity::getStatus, "firing")
                .and(w -> w.isNull(AlertRecordEntity::getClosed).or().eq(AlertRecordEntity::getClosed, 0))
                .and(w -> w.isNull(AlertRecordEntity::getSuppressedUntil).or().le(AlertRecordEntity::getSuppressedUntil, new Date()));
        } else if ("manual".equalsIgnoreCase(statusFilter)) {
            wrapper.eq(AlertRecordEntity::getClosed, 1);
        } else if ("auto".equalsIgnoreCase(statusFilter) || "resolved".equalsIgnoreCase(statusFilter)) {
            wrapper.and(w -> w
                    .eq(AlertRecordEntity::getStatus, "resolved")
                    .or()
                    .eq(AlertRecordEntity::getSeverity, "recover"))
                .and(w -> w.isNull(AlertRecordEntity::getClosed).or().eq(AlertRecordEntity::getClosed, 0));
        }
        wrapper.last("limit 1000");
    }

    private void applyHistoryFilters(LambdaQueryWrapper<AlertRecordEntity> wrapper, Date startTime, Date endTime) {
        Date effectiveStart = startTime;
        if (effectiveStart == null) {
            effectiveStart = Date.from(Instant.now().minusSeconds(7L * 24 * 3600));
        }
        wrapper.ge(AlertRecordEntity::getStartsAt, effectiveStart);
        wrapper.le(endTime != null, AlertRecordEntity::getStartsAt, endTime);
        wrapper.last("limit 5000");
    }

    private List<AlertProblemRsp> enrichAndFilterProblemRecords(List<AlertRecordEntity> records,
                                                                AlertProblemPageReq request) {
        String category = toStr(request.getCategory(), "realtime");
        String deviceType = toStr(request.getDeviceType());
        String hostName = toStr(request.getHostName());
        String ackStatus = toStr(request.getAckStatus());
        String statusFilter = toStr(request.getStatusFilter());

        Map<String, HostInfo> hostMap = loadHostInfoMap();
        List<Long> recordIds = records.stream().map(AlertRecordEntity::getId).collect(Collectors.toList());
        Map<Long, Boolean> ackMap = alertRecordActionService.loadAckMap(recordIds);
        Map<Long, ActionMeta> actionMetaMap = loadActionMetaMap(records);
        Map<Long, AlertNotifyLogEntity> latestNotifyMap = alertNotifyLogService.loadLatestByRecordIds(recordIds);
        Set<String> alertNames = records.stream().map(AlertRecordEntity::getAlertName).filter(StrUtil::isNotBlank).collect(Collectors.toSet());
        Set<String> instanceSet = records.stream().map(AlertRecordEntity::getInstance).filter(StrUtil::isNotBlank).collect(Collectors.toSet());
        Map<String, AlertNotifyLogEntity> latestNotifyByAlert = alertNotifyLogService.loadLatestByAlerts(new ArrayList<>(alertNames), new ArrayList<>(instanceSet));

        List<AlertProblemRsp> all = new ArrayList<>();
        for (AlertRecordEntity record : records) {
            AlertNotifyLogEntity notifyLog = latestNotifyMap.get(record.getId());
            if (notifyLog == null) {
                notifyLog = latestNotifyByAlert.get(buildAlertKey(record.getAlertName(), record.getInstance()));
            }
            AlertProblemRsp dto = toProblemDTO(record, hostMap, ackMap.get(record.getId()), notifyLog, actionMetaMap.get(record.getId()));
            if (!matchesCategory(dto, category, statusFilter)
                || !matchesDeviceType(dto, deviceType)
                || !matchesHostName(dto, hostName)
                || !matchesAck(dto, ackStatus)
                || !matchesStatus(dto, statusFilter)) {
                continue;
            }
            all.add(dto);
        }
        return all;
    }

    private PageData<AlertProblemRsp> paginateProblemResults(List<AlertProblemRsp> all, int page, int limit) {
        int total = all.size();
        int firingCount = 0;
        int resolvedCount = 0;
        for (AlertProblemRsp dto : all) {
            String s = StrUtil.blankToDefault(dto.getStatus(), "").toLowerCase();
            if ("auto".equals(s) || "manual".equals(s) || "resolved".equals(s)) {
                resolvedCount++;
            } else {
                firingCount++;
            }
        }
        int start = Math.max((page - 1) * limit, 0);
        int end = Math.min(start + limit, total);
        List<AlertProblemRsp> list = start >= total ? new ArrayList<>() : all.subList(start, end);
        PageData<AlertProblemRsp> pageData = new PageData<>(list, total);
        pageData.setFiringCount(firingCount);
        pageData.setResolvedCount(resolvedCount);
        return pageData;
    }

    private void sendRecoveryMail(AlertRecordEntity record, String message) {
        Map<String, Object> payload = new HashMap<>();
        payload.put("receiver", "manual-close");
        payload.put("status", "resolved");

        Map<String, Object> commonLabels = new HashMap<>();
        commonLabels.put("alertname", record.getAlertName());
        commonLabels.put("instance", record.getInstance());
        commonLabels.put("severity", "recover");
        payload.put("commonLabels", commonLabels);

        Map<String, Object> commonAnnotations = new HashMap<>();
        commonAnnotations.put("summary", StrUtil.blankToDefault(record.getSummary(), record.getAlertName()));
        commonAnnotations.put("description", StrUtil.blankToDefault(message, "控制台手动关闭告警"));
        payload.put("commonAnnotations", commonAnnotations);

        Map<String, Object> labels = new HashMap<>(commonLabels);
        Map<String, Object> annotations = new HashMap<>(commonAnnotations);
        Map<String, Object> alert = new HashMap<>();
        alert.put("status", "resolved");
        alert.put("labels", labels);
        alert.put("annotations", annotations);
        alert.put("startsAt", record.getStartsAt() == null ? Instant.now().minusSeconds(60).toString() : record.getStartsAt().toInstant().toString());
        alert.put("endsAt", Instant.now().toString());

        List<Map<String, Object>> alerts = new ArrayList<>();
        alerts.add(alert);
        payload.put("alerts", alerts);

        alertTriggerService.notifyFromWebhook(payload, JSONUtil.toJsonStr(payload), "recover");
    }

    private AlertRecordEntity requireRecord(Long recordId) {
        if (recordId == null) {
            throw new ServiceException("告警记录不存在");
        }
        AlertRecordEntity record = this.getById(recordId);
        if (record == null) {
            throw new ServiceException("告警记录不存在");
        }
        return record;
    }

    private void saveAction(Long recordId, String action, String message, String details) {
        alertRecordActionService.saveAction(recordId, action, message, details);
    }

    /**
     * 统一的严重级别规范化方法
     * @param severity 原始严重级别
     * @param returnNullIfUnknown true: 未知级别返回 null; false: 未知级别返回原值
     * @return 规范化后的严重级别
     */
    private String normalizeSeverity(String severity, boolean returnNullIfUnknown) {
        if (StrUtil.isBlank(severity)) {
            return returnNullIfUnknown ? null : severity;
        }
        String value = severity.trim().toLowerCase();

        // 信息级别
        if ("信息".equals(value) || "info".equals(value)) {
            return "info";
        }
        // 重要/警告级别
        if ("重要".equals(value) || "warning".equals(value)) {
            return "warning";
        }
        // 灾难/严重级别
        if ("灾难".equals(value) || "critical".equals(value)) {
            return "critical";
        }
        // 恢复级别
        if ("恢复".equals(value) || "recover".equals(value) || "resolved".equals(value)) {
            return "recover";
        }

        return returnNullIfUnknown ? null : value;
    }

    /**
     * 规范化严重级别（用于数据保存，未知级别返回 null）
     */
    private String normalizeSeverity(String severity) {
        return normalizeSeverity(severity, true);
    }

    private boolean isSuppressed(String alertName, String instance) {
        if (StrUtil.isBlank(alertName) || StrUtil.isBlank(instance)) {
            return false;
        }
        return this.count(
            new LambdaQueryWrapper<AlertRecordEntity>()
                .eq(AlertRecordEntity::getAlertName, alertName)
                .eq(AlertRecordEntity::getInstance, instance)
                .isNotNull(AlertRecordEntity::getSuppressedUntil)
                .gt(AlertRecordEntity::getSuppressedUntil, new Date())
        ) > 0;
    }

    private Map<String, HostInfo> loadHostInfoMap() {
        Map<String, HostInfo> map = new HashMap<>();

        // 只查询必要字段，添加上限保护
        List<LinuxHostEntity> linuxList = linuxHostMapper.selectList(
            new LambdaQueryWrapper<LinuxHostEntity>()
                .select(LinuxHostEntity::getInstance, LinuxHostEntity::getName)
                .last("LIMIT 10000")
        );
        for (LinuxHostEntity item : linuxList) {
            putHost(map, item.getInstance(), item.getName(), "linux");
        }

        List<WindowHostEntity> winList = windowHostMapper.selectList(
            new LambdaQueryWrapper<WindowHostEntity>()
                .select(WindowHostEntity::getInstance, WindowHostEntity::getName)
                .last("LIMIT 10000")
        );
        for (WindowHostEntity item : winList) {
            putHost(map, item.getInstance(), item.getName(), "windows");
        }

        List<BusinessSystemEntity> businessList = businessSystemMapper.selectList(
            new LambdaQueryWrapper<BusinessSystemEntity>()
                .select(BusinessSystemEntity::getInstance, BusinessSystemEntity::getName)
                .last("LIMIT 10000")
        );
        for (BusinessSystemEntity item : businessList) {
            putHost(map, item.getInstance(), item.getName(), "business");
        }

        log.debug("[告警记录] 加载主机映射, linux={}, windows={}, business={}",
            linuxList.size(), winList.size(), businessList.size());
        return map;
    }

    private void putHost(Map<String, HostInfo> map, String instance, String name, String type) {
        String key = normalizeInstance(instance);
        if (StrUtil.isBlank(key)) {
            return;
        }
        map.put(key, new HostInfo(StrUtil.blankToDefault(name, key), type));
    }

    private String normalizeInstance(String instance) {
        if (StrUtil.isBlank(instance)) {
            return null;
        }
        String value = instance.trim();
        if (value.startsWith("http://")) {
            value = value.substring(7);
        } else if (value.startsWith("https://")) {
            value = value.substring(8);
        }
        int slash = value.indexOf('/');
        if (slash > -1) {
            value = value.substring(0, slash);
        }
        int colon = value.indexOf(':');
        if (colon > -1) {
            value = value.substring(0, colon);
        }
        return value.trim();
    }

    private String resolveHostName(String instance, Map<String, HostInfo> map) {
        if (map == null || map.isEmpty() || StrUtil.isBlank(instance)) {
            return null;
        }
        HostInfo info = map.get(normalizeInstance(instance));
        return info == null ? null : info.name;
    }

    private Map<Long, ActionMeta> loadActionMetaMap(List<AlertRecordEntity> records) {
        Map<Long, ActionMeta> result = new HashMap<>();
        if (records == null || records.isEmpty()) {
            return result;
        }
        List<Long> ids = records.stream().map(AlertRecordEntity::getId).collect(Collectors.toList());
        List<AlertRecordActionEntity> actions = alertRecordActionService.list(
            new LambdaQueryWrapper<AlertRecordActionEntity>()
                .select(AlertRecordActionEntity::getRecordId,
                    AlertRecordActionEntity::getAction,
                    AlertRecordActionEntity::getMessage,
                    AlertRecordActionEntity::getCreator,
                    AlertRecordActionEntity::getCreateDate)
                .in(AlertRecordActionEntity::getRecordId, ids)
                .orderByDesc(AlertRecordActionEntity::getCreateDate)
        );
        if (actions.isEmpty()) {
            return result;
        }
        List<Long> userIds = actions.stream()
            .map(AlertRecordActionEntity::getCreator)
            .filter(Objects::nonNull)
            .distinct()
            .collect(Collectors.toList());
        Map<Long, String> userMap = loadUserMap(userIds);
        for (AlertRecordActionEntity action : actions) {
            if (action.getRecordId() == null || StrUtil.isBlank(action.getAction())) {
                continue;
            }
            ActionMeta meta = result.computeIfAbsent(action.getRecordId(), k -> new ActionMeta());
            String operator = userMap.get(action.getCreator());
            if ("确定".equals(action.getAction()) && meta.ackTime == null) {
                meta.ackTime = action.getCreateDate();
                meta.ackOperator = operator;
                meta.ackMessage = action.getMessage();
            }
            if ("关闭".equals(action.getAction()) && meta.closeTime == null) {
                meta.closeTime = action.getCreateDate();
                meta.closeOperator = operator;
                meta.closeMessage = action.getMessage();
            }
        }
        return result;
    }

    private Map<Long, String> loadUserMap(List<Long> userIds) {
        Map<Long, String> userMap = new HashMap<>();
        if (userIds == null || userIds.isEmpty()) {
            return userMap;
        }
        List<SysUserEntity> users = sysUserMapper.selectList(
            new LambdaQueryWrapper<SysUserEntity>()
                .select(SysUserEntity::getId, SysUserEntity::getUsername)
                .in(SysUserEntity::getId, userIds)
        );
        for (SysUserEntity user : users) {
            userMap.put(user.getId(), user.getUsername());
        }
        return userMap;
    }

    private String buildAlertKey(String alertName, String instance) {
        if (StrUtil.isBlank(alertName) || StrUtil.isBlank(instance)) {
            return null;
        }
        return alertName.trim() + "|" + instance.trim();
    }

    private AlertProblemRsp toProblemDTO(AlertRecordEntity record,
                                         Map<String, HostInfo> hostMap,
                                         Boolean acked,
                                         AlertNotifyLogEntity notifyLog,
                                         ActionMeta actionMeta) {
        AlertProblemRsp dto = new AlertProblemRsp();
        dto.setId(record.getId());
        dto.setCreateDate(record.getCreateDate());
        dto.setStartsAt(record.getStartsAt());
        dto.setEndsAt(record.getEndsAt());
        dto.setSeverity(record.getSeverity());
        dto.setInstance(record.getInstance());
        dto.setAlertName(record.getAlertName());
        dto.setSummary(record.getSummary());
        dto.setDescription(record.getDescription());
        dto.setProblem(StrUtil.blankToDefault(record.getDescription(), record.getSummary()));
        dto.setDuration(TimeUtils.formatDurationZh(record.getStartsAt(), record.getEndsAt()));
        dto.setAckStatus(Boolean.TRUE.equals(acked) ? "已确定" : "未确定");

        HostInfo hostInfo = hostMap.get(normalizeInstance(record.getInstance()));
        if (hostInfo == null) {
            dto.setHostType("unknown");
            dto.setHostName(record.getInstance());
        } else {
            dto.setHostType(hostInfo.type);
            dto.setHostName(hostInfo.name);
        }

        if (record.getClosed() != null && record.getClosed() == 1) {
            dto.setStatus("manual");
        } else if ("resolved".equalsIgnoreCase(record.getStatus()) || "recover".equalsIgnoreCase(record.getSeverity())) {
            dto.setStatus("auto");
        } else {
            dto.setStatus("problem");
        }

        if (notifyLog != null) {
            String status = notifyLog.getSendStatus() != null && notifyLog.getSendStatus() == 1 ? "发送成功" : "发送失败";
            String action = "时间:" + notifyLog.getSendTime()
                + " 媒介:" + StrUtil.nullToEmpty(notifyLog.getMediaName())
                + " 接收人:" + StrUtil.nullToEmpty(notifyLog.getReceivers())
                + " 状态:" + status;
            dto.setAction(action);
            dto.setActionTime(notifyLog.getSendTime());
            dto.setActionMedia(notifyLog.getMediaName());
            dto.setActionReceivers(notifyLog.getReceivers());
            dto.setActionSendStatus(status);
        } else {
            dto.setAction("暂无发送记录");
            dto.setActionSendStatus("暂无发送记录");
        }
        if (actionMeta != null) {
            dto.setAckOperator(actionMeta.ackOperator);
            dto.setAckTime(actionMeta.ackTime);
            dto.setAckMessage(actionMeta.ackMessage);
            dto.setCloseOperator(actionMeta.closeOperator);
            dto.setCloseTime(actionMeta.closeTime);
            dto.setCloseMessage(actionMeta.closeMessage);
        }
        return dto;
    }

    private boolean matchesCategory(AlertProblemRsp dto, String category, String statusFilter) {
        if ("history".equalsIgnoreCase(category)) {
            return true;
        }
        return true;
    }

    private boolean matchesDeviceType(AlertProblemRsp dto, String deviceType) {
        if (StrUtil.isBlank(deviceType)) {
            return true;
        }
        return StrUtil.equalsIgnoreCase(deviceType, dto.getHostType());
    }

    private boolean matchesHostName(AlertProblemRsp dto, String hostName) {
        if (StrUtil.isBlank(hostName)) {
            return true;
        }
        String keyword = hostName.toLowerCase();
        return StrUtil.containsIgnoreCase(StrUtil.nullToEmpty(dto.getHostName()), keyword)
            || StrUtil.containsIgnoreCase(StrUtil.nullToEmpty(dto.getInstance()), keyword);
    }

    private boolean matchesAck(AlertProblemRsp dto, String ackStatus) {
        if (StrUtil.isBlank(ackStatus)) {
            return true;
        }
        if ("acked".equalsIgnoreCase(ackStatus)) {
            return "已确定".equals(dto.getAckStatus());
        }
        if ("unacked".equalsIgnoreCase(ackStatus)) {
            return "未确定".equals(dto.getAckStatus());
        }
        return true;
    }

    private boolean matchesStatus(AlertProblemRsp dto, String status) {
        if (StrUtil.isBlank(status)) {
            return true;
        }
        return StrUtil.equalsIgnoreCase(status, dto.getStatus());
    }

    private int parseInt(String value, int defaultValue) {
        try {
            return Integer.parseInt(value);
        } catch (Exception e) {
            log.debug("[告警记录] 整数解析失败, value={}, 使用默认值: {}", value, defaultValue);
            return defaultValue;
        }
    }

    private List<String> listInstancesByType(String deviceType) {
        if (StrUtil.isBlank(deviceType)) {
            return new ArrayList<>();
        }
        String type = deviceType.trim().toLowerCase();
        if ("linux".equals(type)) {
            List<LinuxHostEntity> list = linuxHostMapper.selectList(new LambdaQueryWrapper<LinuxHostEntity>().select(LinuxHostEntity::getInstance));
            return list.stream().map(LinuxHostEntity::getInstance).filter(StrUtil::isNotBlank).distinct().collect(Collectors.toList());
        }
        if ("windows".equals(type)) {
            List<WindowHostEntity> list = windowHostMapper.selectList(new LambdaQueryWrapper<WindowHostEntity>().select(WindowHostEntity::getInstance));
            return list.stream().map(WindowHostEntity::getInstance).filter(StrUtil::isNotBlank).distinct().collect(Collectors.toList());
        }
        if ("business".equals(type)) {
            List<BusinessSystemEntity> list = businessSystemMapper.selectList(new LambdaQueryWrapper<BusinessSystemEntity>().select(BusinessSystemEntity::getInstance));
            return list.stream().map(BusinessSystemEntity::getInstance).filter(StrUtil::isNotBlank).distinct().collect(Collectors.toList());
        }
        return new ArrayList<>();
    }

    /**
     * 根据主机名关键字查询实例列表
     * 重构后使用函数式接口消除代码重复
     */
    private List<String> listInstancesByHostName(String keyword, String deviceType) {
        if (StrUtil.isBlank(keyword)) {
            return new ArrayList<>();
        }
        String value = keyword.trim();
        List<String> instances = new ArrayList<>();

        // 使用函数式接口定义主机查询策略
        if (StrUtil.isBlank(deviceType) || "linux".equalsIgnoreCase(deviceType)) {
            instances.addAll(queryHostInstances(
                () -> linuxHostMapper.selectList(
                    new LambdaQueryWrapper<LinuxHostEntity>()
                        .select(LinuxHostEntity::getInstance)
                        .like(LinuxHostEntity::getName, value)
                ),
                LinuxHostEntity::getInstance
            ));
        }
        if (StrUtil.isBlank(deviceType) || "windows".equalsIgnoreCase(deviceType)) {
            instances.addAll(queryHostInstances(
                () -> windowHostMapper.selectList(
                    new LambdaQueryWrapper<WindowHostEntity>()
                        .select(WindowHostEntity::getInstance)
                        .like(WindowHostEntity::getName, value)
                ),
                WindowHostEntity::getInstance
            ));
        }
        if (StrUtil.isBlank(deviceType) || "business".equalsIgnoreCase(deviceType)) {
            instances.addAll(queryHostInstances(
                () -> businessSystemMapper.selectList(
                    new LambdaQueryWrapper<BusinessSystemEntity>()
                        .select(BusinessSystemEntity::getInstance)
                        .like(BusinessSystemEntity::getName, value)
                ),
                BusinessSystemEntity::getInstance
            ));
        }

        return instances.stream().filter(StrUtil::isNotBlank).distinct().collect(Collectors.toList());
    }

    /**
     * 通用主机实例查询方法，消除重复代码
     * @param querySupplier 查询供应商（Lambda 表达式）
     * @param instanceExtractor 实例提取器（方法引用）
     * @param <T> 主机实体类型
     * @return 实例列表
     */
    private <T> List<String> queryHostInstances(
        java.util.function.Supplier<List<T>> querySupplier,
        java.util.function.Function<T, String> instanceExtractor
    ) {
        return querySupplier.get().stream()
            .map(instanceExtractor)
            .filter(StrUtil::isNotBlank)
            .collect(Collectors.toList());
    }

    private void applyInstanceLikeFilter(LambdaQueryWrapper<AlertRecordEntity> wrapper, List<String> instances) {
        List<String> keys = buildInstanceLikeKeys(instances);
        if (keys.isEmpty()) {
            wrapper.eq(AlertRecordEntity::getId, -1L);
            return;
        }
        wrapper.and(w -> {
            boolean first = true;
            for (String key : keys) {
                if (first) {
                    w.like(AlertRecordEntity::getInstance, key);
                    first = false;
                } else {
                    w.or().like(AlertRecordEntity::getInstance, key);
                }
            }
        });
    }

    private List<String> buildInstanceLikeKeys(List<String> instances) {
        if (instances == null || instances.isEmpty()) {
            return new ArrayList<>();
        }
        List<String> keys = new ArrayList<>(instances.size() * 2);
        for (String instance : instances) {
            if (StrUtil.isBlank(instance)) {
                continue;
            }
            keys.add(instance);
            String normalized = normalizeInstance(instance);
            if (StrUtil.isNotBlank(normalized) && !normalized.equals(instance)) {
                keys.add(normalized);
            }
        }
        return keys.stream().filter(StrUtil::isNotBlank).distinct().collect(Collectors.toList());
    }

    private List<String> parseSeverityList(String severity) {
        if (StrUtil.isBlank(severity)) {
            return new ArrayList<>();
        }
        String[] parts = severity.split(",");
        List<String> result = new ArrayList<>(parts.length);
        for (String s : parts) {
            String normalized = normalizeSeverityForQuery(s.trim());
            if (StrUtil.isNotBlank(normalized)) {
                result.add(normalized);
            }
        }
        return result.stream().distinct().collect(Collectors.toList());
    }

    /**
     * 规范化严重级别（用于查询，未知级别返回原值）
     */
    private String normalizeSeverityForQuery(String severity) {
        return normalizeSeverity(severity, false);
    }

    private static class HostInfo {
        private final String name;
        private final String type;

        private HostInfo(String name, String type) {
            this.name = name;
            this.type = type;
        }
    }

    private static class ActionMeta {
        private String ackOperator;
        private Date ackTime;
        private String ackMessage;
        private String closeOperator;
        private Date closeTime;
        private String closeMessage;
    }

    private String resolveSeverity(Map<String, Object> payload, Map<String, Object> alert, String fallback) {
        String alertStatus = alert == null ? null : toStr(alert.get("status"));
        if ("resolved".equalsIgnoreCase(alertStatus)) {
            return "recover";
        }
        Map<String, Object> labels = toMap(alert == null ? null : alert.get("labels"));
        String severity = getValue(labels, toMap(payload.get("commonLabels")), "severity");
        if (StrUtil.isNotBlank(severity)) {
            return severity;
        }
        Object status = payload == null ? null : payload.get("status");
        if ("resolved".equalsIgnoreCase(String.valueOf(status))) {
            return "recover";
        }
        return fallback;
    }

    private static String toStr(Object value) {
        return value == null ? null : String.valueOf(value);
    }

    private static String toStr(Object value, String fallback) {
        String str = toStr(value);
        return StrUtil.isNotBlank(str) ? str : fallback;
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> toMap(Object value) {
        return value instanceof Map ? (Map<String, Object>) value : null;
    }

    private static String getValue(Map<String, Object> primary, Map<String, Object> fallback, String key) {
        String val = primary != null ? toStr(primary.get(key)) : null;
        if (StrUtil.isNotBlank(val)) {
            return val;
        }
        return fallback != null ? toStr(fallback.get(key)) : null;
    }

    private static String getValue(Map<String, Object> primary, Map<String, Object> fallback, String key, String extraFallback) {
        String val = getValue(primary, fallback, key);
        if (StrUtil.isNotBlank(val)) {
            return val;
        }
        return extraFallback;
    }
}
