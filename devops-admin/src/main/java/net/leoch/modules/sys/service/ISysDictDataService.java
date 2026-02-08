package net.leoch.modules.sys.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.page.PageData;
import net.leoch.modules.sys.dto.SysDictDataDTO;
import net.leoch.modules.sys.dto.SysDictDataPageRequest;
import net.leoch.modules.sys.entity.SysDictDataEntity;

/**
 * 数据字典
 *
 * @author Taohongqiang
 */
public interface ISysDictDataService extends IService<SysDictDataEntity> {

    PageData<SysDictDataDTO> page(SysDictDataPageRequest request);

    SysDictDataDTO get(Long id);

    void save(SysDictDataDTO dto);

    void update(SysDictDataDTO dto);

    void delete(Long[] ids);

}
