package net.leoch.modules.alert.service;

import net.leoch.common.service.CrudService;
import net.leoch.modules.alert.dto.AlertTriggerDTO;
import net.leoch.modules.alert.entity.AlertTriggerEntity;

import java.util.List;
import java.util.Map;

/**
 * 告警触发器
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
public interface AlertTriggerService extends CrudService<AlertTriggerEntity, AlertTriggerDTO> {

    void fillReceiverUserIdList(AlertTriggerDTO dto);

    void fillReceiverUserIdList(List<AlertTriggerDTO> list);

    void notifyFromWebhook(Map<String, Object> payload, String rawJson, String severity);

    void sendTest(Long templateId, Long triggerId, String rawJson);
}
