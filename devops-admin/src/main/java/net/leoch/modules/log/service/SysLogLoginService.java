

package net.leoch.modules.log.service;

import net.leoch.common.page.PageData;
import net.leoch.common.service.BaseService;
import net.leoch.modules.log.dto.SysLogLoginDTO;
import net.leoch.modules.log.entity.SysLogLoginEntity;

import java.util.List;
import java.util.Map;

/**
 * 登录日志
 *
 * @author Taohongqiang
 * @since 1.0.0
 */
public interface SysLogLoginService extends BaseService<SysLogLoginEntity> {

    PageData<SysLogLoginDTO> page(Map<String, Object> params);

    List<SysLogLoginDTO> list(Map<String, Object> params);

    void save(SysLogLoginEntity entity);
}