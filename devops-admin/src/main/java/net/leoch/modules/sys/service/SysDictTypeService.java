package net.leoch.modules.sys.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.page.PageData;
import net.leoch.modules.sys.dto.SysDictTypeDTO;
import net.leoch.modules.sys.dto.SysDictTypePageRequest;
import net.leoch.modules.sys.entity.DictType;
import net.leoch.modules.sys.entity.SysDictTypeEntity;

import java.util.List;

/**
 * 数据字典
 *
 * @author Taohongqiang
 */
public interface SysDictTypeService extends IService<SysDictTypeEntity> {

    PageData<SysDictTypeDTO> page(SysDictTypePageRequest request);

    SysDictTypeDTO get(Long id);

    void save(SysDictTypeDTO dto);

    void update(SysDictTypeDTO dto);

    void delete(Long[] ids);

    /**
     * 获取所有字典
     */
    List<DictType> getAllList();

}
