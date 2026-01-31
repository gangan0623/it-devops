

package net.leoch.modules.log.service;

import net.leoch.common.page.PageData;
import net.leoch.common.service.BaseService;
import net.leoch.modules.log.dto.SysLogOperationDTO;
import net.leoch.modules.log.entity.SysLogOperationEntity;

import java.util.List;
import java.util.Map;

/**
 * 操作日志
 *
 * @author Taohongqiang
 * @since 1.0.0
 */
public interface SysLogOperationService extends BaseService<SysLogOperationEntity> {

    PageData<SysLogOperationDTO> page(Map<String, Object> params);

    List<SysLogOperationDTO> list(Map<String, Object> params);

    void save(SysLogOperationEntity entity);
}