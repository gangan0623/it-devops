package net.leoch.modules.alert.service.impl;

import lombok.extern.slf4j.Slf4j;
import net.leoch.framework.config.ops.OnlineStatusConfig;
import net.leoch.modules.alert.service.AlertRealtimeViewService;
import net.leoch.modules.alert.service.IAlertSseService;
import net.leoch.modules.alert.vo.rsp.AlertRealtimeRsp;
import org.springframework.stereotype.Service;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * 实时告警 SSE
 */
@Slf4j
@Service
public class AlertSseServiceImpl implements IAlertSseService {

    private final AlertRealtimeViewService alertRealtimeViewService;
    private final OnlineStatusConfig properties;
    private final CopyOnWriteArrayList<SseEmitter> emitters = new CopyOnWriteArrayList<>();

    public AlertSseServiceImpl(AlertRealtimeViewService alertRealtimeViewService,
                               OnlineStatusConfig properties) {
        this.alertRealtimeViewService = alertRealtimeViewService;
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
        return alertRealtimeViewService.recentAlerts(10);
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

}
