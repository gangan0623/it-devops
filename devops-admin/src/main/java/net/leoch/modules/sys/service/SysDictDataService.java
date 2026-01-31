

package net.leoch.modules.sys.service;

import net.leoch.common.page.PageData;
import net.leoch.common.service.BaseService;
import net.leoch.modules.sys.dto.SysDictDataDTO;
import net.leoch.modules.sys.entity.SysDictDataEntity;

import java.util.Map;

/**
 * 数据字典
 *
 * @author Taohongqiang
 */
public interface SysDictDataService extends BaseService<SysDictDataEntity> {

    PageData<SysDictDataDTO> page(Map<String, Object> params);

    SysDictDataDTO get(Long id);

    void save(SysDictDataDTO dto);

    void update(SysDictDataDTO dto);

    void delete(Long[] ids);

}