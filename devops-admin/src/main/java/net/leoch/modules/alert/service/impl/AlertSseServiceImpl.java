package net.leoch.modules.alert.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.extern.slf4j.Slf4j;
import net.leoch.framework.config.ops.OnlineStatusConfig;
import net.leoch.modules.alert.entity.AlertRecordEntity;
import net.leoch.modules.alert.mapper.AlertRecordMapper;
import net.leoch.modules.alert.service.IAlertSseService;
import net.leoch.modules.alert.vo.rsp.AlertRealtimeRsp;
import net.leoch.modules.ops.entity.BusinessSystemEntity;
import net.leoch.modules.ops.entity.LinuxHostEntity;
import net.leoch.modules.ops.entity.WindowHostEntity;
import net.leoch.modules.ops.mapper.BusinessSystemMapper;
import net.leoch.modules.ops.mapper.LinuxHostMapper;
import net.leoch.modules.ops.mapper.WindowHostMapper;
import org.springframework.stereotype.Service;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * 实时告警 SSE
 */
@Slf4j
@Service
public class AlertSseServiceImpl implements IAlertSseService {

    private final AlertRecordMapper alertRecordMapper;
    private final LinuxHostMapper linuxHostMapper;
    private final WindowHostMapper windowHostMapper;
    private final BusinessSystemMapper businessSystemMapper;
    private final OnlineStatusConfig properties;
    private final CopyOnWriteArrayList<SseEmitter> emitters = new CopyOnWriteArrayList<>();

    public AlertSseServiceImpl(AlertRecordMapper alertRecordMapper,
                               LinuxHostMapper linuxHostMapper,
                               WindowHostMapper windowHostMapper,
                               BusinessSystemMapper businessSystemMapper,
                               OnlineStatusConfig properties) {
        this.alertRecordMapper = alertRecordMapper;
        this.linuxHostMapper = linuxHostMapper;
        this.windowHostMapper = windowHostMapper;
        this.businessSystemMapper = businessSystemMapper;
        this.properties = properties;
    }

    @Override
    public SseEmitter createEmitter() {
        SseEmitter emitter = new SseEmitter(properties.getSse().getEmitterTimeout());
        emitters.add(emitter);
        log.debug("[告警SSE] 创建连接, 当前连接数={}", emitters.size());
        emitter.onCompletion(() -> {
            emitters.remove(emitter);
            log.debug("[告警SSE] 连接完成, 当前连接数={}", emitters.size());
        });
        emitter.onTimeout(() -> {
            emitters.remove(emitter);
            log.debug("[告警SSE] 连接超时, 当前连接数={}", emitters.size());
        });
        emitter.onError((ex) -> {
            emitters.remove(emitter);
            log.debug("[告警SSE] 连接错误, 当前连接数={}, error={}", emitters.size(), ex.getMessage());
        });
        try {
            emitter.send(SseEmitter.event().name("recentAlerts").data(recentAlerts()));
        } catch (IOException e) {
            emitters.remove(emitter);
            log.warn("[告警SSE] 发送初始数据失败, error={}", e.getMessage());
        }
        return emitter;
    }

    @Override
    public List<AlertRealtimeRsp> recentAlerts() {
        List<AlertRecordEntity> list = alertRecordMapper.selectList(
            new LambdaQueryWrapper<AlertRecordEntity>()
                .select(AlertRecordEntity::getAlertName, AlertRecordEntity::getInstance, AlertRecordEntity::getSeverity,
                    AlertRecordEntity::getStatus, AlertRecordEntity::getStartsAt)
                .and(wrapper -> wrapper.isNull(AlertRecordEntity::getClosed).or().eq(AlertRecordEntity::getClosed, 0))
                .and(wrapper -> wrapper.isNull(AlertRecordEntity::getSuppressedUntil).or().le(AlertRecordEntity::getSuppressedUntil, new java.util.Date()))
                .orderByDesc(AlertRecordEntity::getStartsAt)
                .last("limit 10")
        );
        Map<String, String> hostMap = loadHostMap();
        List<AlertRealtimeRsp> result = new ArrayList<>();
        for (AlertRecordEntity entity : list) {
            AlertRealtimeRsp dto = new AlertRealtimeRsp();
            dto.setAlertName(entity.getAlertName());
            dto.setInstance(entity.getInstance());
            dto.setHostName(hostMap.get(normalizeInstance(entity.getInstance())));
            dto.setSeverity(entity.getSeverity());
            dto.setStatus(entity.getStatus());
            dto.setTime(entity.getStartsAt());
            result.add(dto);
        }
        return result;
    }

    @Override
    public void publishRecentAlerts() {
        if (emitters.isEmpty()) {
            log.debug("[告警SSE] 无活跃连接，跳过推送");
            return;
        }
        List<AlertRealtimeRsp> data = recentAlerts();
        log.debug("[告警SSE] 开始推送告警, 连接数={}, 告警数={}", emitters.size(), data.size());
        int successCount = 0;
        int failCount = 0;
        for (SseEmitter emitter : emitters) {
            try {
                emitter.send(SseEmitter.event().name("recentAlerts").data(data));
                successCount++;
            } catch (IOException e) {
                emitters.remove(emitter);
                failCount++;
                log.debug("[告警SSE] 推送失败，移除连接, error={}", e.getMessage());
            }
        }
        if (failCount > 0) {
            log.info("[告警SSE] 推送完成, 成功={}, 失败={}, 剩余连接={}", successCount, failCount, emitters.size());
        }
    }

    private Map<String, String> loadHostMap() {
        // 预分配容量：假设 Linux/Windows/业务系统各50台，总共150台
        Map<String, String> map = new HashMap<>(150);
        List<LinuxHostEntity> linuxList = linuxHostMapper.selectList(new LambdaQueryWrapper<LinuxHostEntity>().select(LinuxHostEntity::getInstance, LinuxHostEntity::getName));
        for (LinuxHostEntity item : linuxList) {
            putHost(map, item.getInstance(), item.getName());
        }
        List<WindowHostEntity> winList = windowHostMapper.selectList(new LambdaQueryWrapper<WindowHostEntity>().select(WindowHostEntity::getInstance, WindowHostEntity::getName));
        for (WindowHostEntity item : winList) {
            putHost(map, item.getInstance(), item.getName());
        }
        List<BusinessSystemEntity> businessList = businessSystemMapper.selectList(new LambdaQueryWrapper<BusinessSystemEntity>().select(BusinessSystemEntity::getInstance, BusinessSystemEntity::getName));
        for (BusinessSystemEntity item : businessList) {
            putHost(map, item.getInstance(), item.getName());
        }
        return map;
    }

    private void putHost(Map<String, String> map, String instance, String name) {
        String key = normalizeInstance(instance);
        if (StrUtil.isBlank(key)) {
            return;
        }
        map.put(key, StrUtil.blankToDefault(name, key));
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
}
