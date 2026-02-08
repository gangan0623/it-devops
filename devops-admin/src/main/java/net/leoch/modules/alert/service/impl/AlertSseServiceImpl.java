package net.leoch.modules.alert.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import net.leoch.modules.alert.dao.AlertRecordDao;
import net.leoch.modules.alert.dto.AlertRealtimeDTO;
import net.leoch.modules.alert.entity.AlertRecordEntity;
import lombok.extern.slf4j.Slf4j;
import net.leoch.modules.alert.service.AlertSseService;
import net.leoch.modules.ops.dao.BusinessSystemDao;
import net.leoch.modules.ops.dao.LinuxHostDao;
import net.leoch.modules.ops.dao.WindowHostDao;
import net.leoch.modules.ops.entity.BusinessSystemEntity;
import net.leoch.modules.ops.entity.LinuxHostEntity;
import net.leoch.modules.ops.entity.WindowHostEntity;
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
public class AlertSseServiceImpl implements AlertSseService {

    private final AlertRecordDao alertRecordDao;
    private final LinuxHostDao linuxHostDao;
    private final WindowHostDao windowHostDao;
    private final BusinessSystemDao businessSystemDao;
    private final CopyOnWriteArrayList<SseEmitter> emitters = new CopyOnWriteArrayList<>();

    public AlertSseServiceImpl(AlertRecordDao alertRecordDao,
                               LinuxHostDao linuxHostDao,
                               WindowHostDao windowHostDao,
                               BusinessSystemDao businessSystemDao) {
        this.alertRecordDao = alertRecordDao;
        this.linuxHostDao = linuxHostDao;
        this.windowHostDao = windowHostDao;
        this.businessSystemDao = businessSystemDao;
    }

    @Override
    public SseEmitter createEmitter() {
        SseEmitter emitter = new SseEmitter(0L);
        emitters.add(emitter);
        emitter.onCompletion(() -> emitters.remove(emitter));
        emitter.onTimeout(() -> emitters.remove(emitter));
        emitter.onError((ex) -> emitters.remove(emitter));
        try {
            emitter.send(SseEmitter.event().name("recentAlerts").data(recentAlerts()));
        } catch (IOException e) {
            emitters.remove(emitter);
        }
        return emitter;
    }

    @Override
    public List<AlertRealtimeDTO> recentAlerts() {
        List<AlertRecordEntity> list = alertRecordDao.selectList(
            new LambdaQueryWrapper<AlertRecordEntity>()
                .select(AlertRecordEntity::getAlertName, AlertRecordEntity::getInstance, AlertRecordEntity::getSeverity,
                    AlertRecordEntity::getStatus, AlertRecordEntity::getStartsAt)
                .and(wrapper -> wrapper.isNull(AlertRecordEntity::getClosed).or().eq(AlertRecordEntity::getClosed, 0))
                .and(wrapper -> wrapper.isNull(AlertRecordEntity::getSuppressedUntil).or().le(AlertRecordEntity::getSuppressedUntil, new java.util.Date()))
                .orderByDesc(AlertRecordEntity::getStartsAt)
                .last("limit 10")
        );
        Map<String, String> hostMap = loadHostMap();
        List<AlertRealtimeDTO> result = new ArrayList<>();
        for (AlertRecordEntity entity : list) {
            AlertRealtimeDTO dto = new AlertRealtimeDTO();
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
            return;
        }
        List<AlertRealtimeDTO> data = recentAlerts();
        for (SseEmitter emitter : emitters) {
            try {
                emitter.send(SseEmitter.event().name("recentAlerts").data(data));
            } catch (IOException e) {
                emitters.remove(emitter);
                log.debug("SSE发送失败，移除连接: {}", e.getMessage());
            }
        }
    }

    private Map<String, String> loadHostMap() {
        Map<String, String> map = new HashMap<>();
        List<LinuxHostEntity> linuxList = linuxHostDao.selectList(new LambdaQueryWrapper<LinuxHostEntity>().select(LinuxHostEntity::getInstance, LinuxHostEntity::getName));
        for (LinuxHostEntity item : linuxList) {
            putHost(map, item.getInstance(), item.getName());
        }
        List<WindowHostEntity> winList = windowHostDao.selectList(new LambdaQueryWrapper<WindowHostEntity>().select(WindowHostEntity::getInstance, WindowHostEntity::getName));
        for (WindowHostEntity item : winList) {
            putHost(map, item.getInstance(), item.getName());
        }
        List<BusinessSystemEntity> businessList = businessSystemDao.selectList(new LambdaQueryWrapper<BusinessSystemEntity>().select(BusinessSystemEntity::getInstance, BusinessSystemEntity::getName));
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
