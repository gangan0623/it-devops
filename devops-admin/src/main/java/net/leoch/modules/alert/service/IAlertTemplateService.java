package net.leoch.modules.alert.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.page.PageData;
import net.leoch.modules.alert.dto.AlertTemplateDTO;
import net.leoch.modules.alert.dto.AlertTemplatePageRequest;
import net.leoch.modules.alert.entity.AlertTemplateEntity;

import java.util.List;

/**
 * 告警模板
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
public interface IAlertTemplateService extends IService<AlertTemplateEntity> {
    PageData<AlertTemplateDTO> page(AlertTemplatePageRequest request);
    List<AlertTemplateDTO> list(AlertTemplatePageRequest request);
    AlertTemplateDTO get(Long id);
    void save(AlertTemplateDTO dto);
    void update(AlertTemplateDTO dto);
    void delete(Long[] ids);
}
