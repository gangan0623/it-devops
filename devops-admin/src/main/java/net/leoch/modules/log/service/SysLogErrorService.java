

package net.leoch.modules.log.service;


import net.leoch.common.page.PageData;
import net.leoch.common.service.BaseService;
import net.leoch.modules.log.dto.SysLogErrorDTO;
import net.leoch.modules.log.entity.SysLogErrorEntity;

import java.util.List;
import java.util.Map;

/**
 * 异常日志
 *
 * @author Taohongqiang
 * @since 1.0.0
 */
public interface SysLogErrorService extends BaseService<SysLogErrorEntity> {

    PageData<SysLogErrorDTO> page(Map<String, Object> params);

    List<SysLogErrorDTO> list(Map<String, Object> params);

    void save(SysLogErrorEntity entity);

}