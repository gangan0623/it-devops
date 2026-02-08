package net.leoch.modules.alert.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.modules.alert.entity.AlertNotifyLogEntity;

import java.util.List;
import java.util.Map;

/**
 * 告警发送日志
 *
 * @author Taohongqiang
 */
public interface IAlertNotifyLogService extends IService<AlertNotifyLogEntity> {

    /**
     * 根据告警记录ID列表查询最新通知记录（每个recordId取最新一条）
     */
    Map<Long, AlertNotifyLogEntity> loadLatestByRecordIds(List<Long> recordIds);

    /**
     * 根据告警名+实例查询最新通知记录
     */
    Map<String, AlertNotifyLogEntity> loadLatestByAlerts(List<String> alertNames, List<String> instances);
}
