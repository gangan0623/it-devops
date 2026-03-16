package net.leoch.modules.alert.service.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import net.leoch.modules.alert.entity.AlertRecordEntity;

import java.util.Map;

/**
 * Webhook单条告警与当前态记录的关联结果
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class AlertWebhookRecordResult {
    private Map<String, Object> alert;
    private AlertRecordEntity record;
}
