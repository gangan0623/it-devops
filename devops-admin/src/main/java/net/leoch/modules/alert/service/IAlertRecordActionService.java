package net.leoch.modules.alert.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.modules.alert.vo.rsp.AlertRecordActionRsp;
import net.leoch.modules.alert.entity.AlertRecordActionEntity;

import java.util.List;
import java.util.Map;

/**
 * 告警记录操作历史
 *
 * @author Taohongqiang
 */
public interface IAlertRecordActionService extends IService<AlertRecordActionEntity> {

    /**
     * 查询操作历史（含操作人名称）
     */
    List<AlertRecordActionRsp> listByRecordId(Long recordId);

    /**
     * 保存操作记录
     */
    void saveAction(Long recordId, String action, String message, String details);

    /**
     * 根据记录ID列表查询确认状态
     */
    Map<Long, Boolean> loadAckMap(List<Long> recordIds);
}
