package net.leoch.modules.alert.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import net.leoch.modules.alert.dao.AlertRecordDao;
import net.leoch.modules.alert.dto.AlertRealtimeDTO;
import net.leoch.modules.alert.entity.AlertRecordEntity;
import net.leoch.modules.alert.service.AlertSseService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * 实时告警 SSE
 */
@Service
public class AlertSseServiceImpl implements AlertSseService {

    private static final Logger logger = LoggerFactory.getLogger(AlertSseServiceImpl.class);
    private final AlertRecordDao alertRecordDao;
    private final CopyOnWriteArrayList<SseEmitter> emitters = new CopyOnWriteArrayList<>();

    public AlertSseServiceImpl(AlertRecordDao alertRecordDao) {
        this.alertRecordDao = alertRecordDao;
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
                .orderByDesc(AlertRecordEntity::getStartsAt)
                .last("limit 10")
        );
        List<AlertRealtimeDTO> result = new ArrayList<>();
        for (AlertRecordEntity entity : list) {
            AlertRealtimeDTO dto = new AlertRealtimeDTO();
            dto.setAlertName(entity.getAlertName());
            dto.setInstance(entity.getInstance());
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
                logger.debug("SSE发送失败，移除连接: {}", e.getMessage());
            }
        }
    }
}
