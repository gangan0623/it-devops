package net.leoch.modules.alert.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.page.PageData;
import net.leoch.modules.alert.vo.req.AlertTriggerPageReq;
import net.leoch.modules.alert.vo.req.AlertTriggerReq;
import net.leoch.modules.alert.vo.rsp.AlertTriggerRsp;
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
    PageData<AlertTriggerRsp> page(AlertTriggerPageReq request);
    PageData<AlertTriggerRsp> pageWithReceivers(AlertTriggerPageReq request);
    List<AlertTriggerRsp> list(AlertTriggerPageReq request);
    AlertTriggerRsp get(Long id);
    AlertTriggerRsp getWithReceivers(Long id);
    List<Map<String, Object>> options();
    void save(AlertTriggerReq dto);
    void update(AlertTriggerReq dto);
    void delete(Long[] ids);
    void fillReceiverUserIdList(AlertTriggerRsp dto);
    void fillReceiverUserIdList(List<AlertTriggerRsp> list);
    void notifyFromWebhook(Map<String, Object> payload, String rawJson, String severity);
    void sendTest(Long templateId, Long triggerId, String rawJson);
    Map<String, Object> resources();
}
