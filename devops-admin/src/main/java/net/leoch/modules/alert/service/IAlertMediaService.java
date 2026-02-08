package net.leoch.modules.alert.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.page.PageData;
import net.leoch.modules.alert.vo.rsp.AlertMediaRsp;
import net.leoch.modules.alert.vo.req.AlertMediaPageReq;
import net.leoch.modules.alert.entity.AlertMediaEntity;

import java.util.List;

/**
 * 告警媒介
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
public interface IAlertMediaService extends IService<AlertMediaEntity> {
    PageData<AlertMediaRsp> page(AlertMediaPageReq request);
    List<AlertMediaRsp> list(AlertMediaPageReq request);
    AlertMediaRsp get(Long id);
    void save(AlertMediaRsp dto);
    void update(AlertMediaRsp dto);
    void delete(Long[] ids);
}
