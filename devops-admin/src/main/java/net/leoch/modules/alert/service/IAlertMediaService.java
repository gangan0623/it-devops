package net.leoch.modules.alert.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.page.PageData;
import net.leoch.modules.alert.dto.AlertMediaDTO;
import net.leoch.modules.alert.dto.AlertMediaPageRequest;
import net.leoch.modules.alert.entity.AlertMediaEntity;

import java.util.List;

/**
 * 告警媒介
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
public interface IAlertMediaService extends IService<AlertMediaEntity> {
    PageData<AlertMediaDTO> page(AlertMediaPageRequest request);
    List<AlertMediaDTO> list(AlertMediaPageRequest request);
    AlertMediaDTO get(Long id);
    void save(AlertMediaDTO dto);
    void update(AlertMediaDTO dto);
    void delete(Long[] ids);
}
