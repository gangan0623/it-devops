package net.leoch.modules.alert.service;

import net.leoch.common.service.CrudService;
import net.leoch.modules.alert.dto.AlertRecordActionDTO;
import net.leoch.modules.alert.dto.AlertProblemDTO;
import net.leoch.modules.alert.dto.AlertRecordDTO;
import net.leoch.modules.alert.entity.AlertRecordEntity;

import java.util.List;
import java.util.Map;
import net.leoch.common.page.PageData;

/**
 * 告警记录
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
public interface AlertRecordService extends CrudService<AlertRecordEntity, AlertRecordDTO> {

    void saveFromWebhook(Map<String, Object> payload, String rawJson, String severity);

    List<AlertRecordActionDTO> history(Long recordId);

    void changeSeverity(Long recordId, String severity, String message);

    void suppress(Long recordId, Integer days, String message);

    void acknowledge(Long recordId, String message);

    void close(Long recordId, String message);

    PageData<AlertProblemDTO> problemPage(Map<String, Object> params);
}
