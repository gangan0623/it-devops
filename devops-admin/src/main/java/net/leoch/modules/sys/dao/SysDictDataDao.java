

package net.leoch.modules.sys.dao;

import net.leoch.common.dao.BaseDao;
import net.leoch.modules.sys.entity.DictData;
import net.leoch.modules.sys.entity.SysDictDataEntity;
import org.apache.ibatis.annotations.Mapper;

import java.util.List;

/**
 * 字典数据
 *
 * @author Taohongqiang
 */
@Mapper
public interface SysDictDataDao extends BaseDao<SysDictDataEntity> {

    /**
     * 字典数据列表
     */
    List<DictData> getDictDataList();
}
