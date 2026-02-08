package net.leoch.modules.alert.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.page.PageData;
import net.leoch.modules.alert.vo.rsp.AlertTemplateRsp;
import net.leoch.modules.alert.vo.req.AlertTemplatePageReq;
import net.leoch.modules.alert.entity.AlertTemplateEntity;

import java.util.List;

/**
 * 告警模板
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
public interface IAlertTemplateService extends IService<AlertTemplateEntity> {
    PageData<AlertTemplateRsp> page(AlertTemplatePageReq request);
    List<AlertTemplateRsp> list(AlertTemplatePageReq request);
    AlertTemplateRsp get(Long id);
    void save(AlertTemplateRsp dto);
    void update(AlertTemplateRsp dto);
    void delete(Long[] ids);
}
