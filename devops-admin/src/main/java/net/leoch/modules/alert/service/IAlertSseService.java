package net.leoch.modules.alert.service;

import net.leoch.modules.alert.vo.rsp.AlertRealtimeRsp;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.util.List;

/**
 * 实时告警 SSE
 */
public interface IAlertSseService {
    SseEmitter createEmitter();

    List<AlertRealtimeRsp> recentAlerts();

    void publishRecentAlerts();
}
