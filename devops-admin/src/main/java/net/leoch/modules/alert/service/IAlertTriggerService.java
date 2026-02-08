package net.leoch.modules.alert.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.page.PageData;
import net.leoch.modules.alert.dto.AlertTriggerDTO;
import net.leoch.modules.alert.dto.AlertTriggerPageRequest;
import net.leoch.modules.alert.entity.AlertTriggerEntity;

import java.util.List;
import java.util.Map;

/**
 * 告警触发器
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
public interface IAlertTriggerService extends IService<AlertTriggerEntity> {
    PageData<AlertTriggerDTO> page(AlertTriggerPageRequest request);
    List<AlertTriggerDTO> list(AlertTriggerPageRequest request);
    AlertTriggerDTO get(Long id);
    void save(AlertTriggerDTO dto);
    void update(AlertTriggerDTO dto);
    void delete(Long[] ids);
    void fillReceiverUserIdList(AlertTriggerDTO dto);
    void fillReceiverUserIdList(List<AlertTriggerDTO> list);
    void notifyFromWebhook(Map<String, Object> payload, String rawJson, String severity);
    void sendTest(Long templateId, Long triggerId, String rawJson);
    Map<String, Object> resources();
}
