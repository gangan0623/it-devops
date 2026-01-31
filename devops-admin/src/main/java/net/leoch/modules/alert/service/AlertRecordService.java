package net.leoch.modules.alert.service;

import net.leoch.common.service.CrudService;
import net.leoch.modules.alert.dto.AlertRecordDTO;
import net.leoch.modules.alert.entity.AlertRecordEntity;

import java.util.Map;

/**
 * 告警记录
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
public interface AlertRecordService extends CrudService<AlertRecordEntity, AlertRecordDTO> {

    void saveFromWebhook(Map<String, Object> payload, String rawJson, String severity);
}
