package net.leoch.modules.alert.service;

import net.leoch.modules.alert.dto.AlertRealtimeDTO;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.util.List;

/**
 * 实时告警 SSE
 */
public interface AlertSseService {
    SseEmitter createEmitter();

    List<AlertRealtimeDTO> recentAlerts();

    void publishRecentAlerts();
}
