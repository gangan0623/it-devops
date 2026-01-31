

package net.leoch.modules.sys.dao;

import net.leoch.common.dao.BaseDao;
import net.leoch.modules.sys.entity.DictType;
import net.leoch.modules.sys.entity.SysDictTypeEntity;
import org.apache.ibatis.annotations.Mapper;

import java.util.List;

/**
 * 字典类型
 *
 * @author Taohongqiang
 */
@Mapper
public interface SysDictTypeDao extends BaseDao<SysDictTypeEntity> {

    /**
     * 字典类型列表
     */
    List<DictType> getDictTypeList();

}
