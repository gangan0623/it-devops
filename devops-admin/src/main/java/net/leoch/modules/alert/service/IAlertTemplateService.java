package net.leoch.modules.alert.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.data.page.PageData;
import net.leoch.modules.alert.vo.req.AlertTemplatePageReq;
import net.leoch.modules.alert.vo.req.AlertTemplateReq;
import net.leoch.modules.alert.vo.rsp.AlertTemplateRsp;
import net.leoch.modules.alert.vo.req.AlertTemplatePreviewReq;
import net.leoch.modules.alert.entity.AlertTemplateEntity;

import java.util.List;
import java.util.Map;

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
    void save(AlertTemplateReq dto);
    void update(AlertTemplateReq dto);
    void delete(Long[] ids);

    /**
     * 模板预览
     * @param req 预览请求
     * @return 渲染后的 subject 和 html
     */
    Map<String, Object> preview(AlertTemplatePreviewReq req);
}
