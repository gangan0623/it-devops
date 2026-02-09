package net.leoch.modules.alert.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.data.page.PageData;
import net.leoch.modules.alert.vo.rsp.AlertRecordActionRsp;
import net.leoch.modules.alert.vo.rsp.AlertProblemRsp;
import net.leoch.modules.alert.vo.rsp.AlertRecordRsp;
import net.leoch.modules.alert.vo.req.AlertProblemPageReq;
import net.leoch.modules.alert.vo.req.AlertRecordPageReq;
import net.leoch.modules.alert.entity.AlertRecordEntity;

import java.util.List;
import java.util.Map;

/**
 * 告警记录
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
public interface IAlertRecordService extends IService<AlertRecordEntity> {
    PageData<AlertRecordRsp> page(AlertRecordPageReq request);
    AlertRecordRsp get(Long id);
    void delete(Long[] ids);
    void saveFromWebhook(Map<String, Object> payload, String rawJson, String severity);
    List<AlertRecordActionRsp> history(Long recordId);
    void changeSeverity(Long recordId, String severity, String message);
    void suppress(Long recordId, Integer days, String message);
    void acknowledge(Long recordId, String message);
    void close(Long recordId, String message);
    PageData<AlertProblemRsp> problemPage(AlertProblemPageReq request);
}
