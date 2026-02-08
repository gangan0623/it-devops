

package net.leoch.modules.sys.dao;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.sys.entity.DictData;
import net.leoch.modules.sys.entity.SysDictDataEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 字典数据
 *
 * @author Taohongqiang
 */
@Mapper
public interface SysDictDataDao extends BaseMapper<SysDictDataEntity> {

    /**
     * 字典数据列表
     */
    List<DictData> getDictDataList();

    /**
     * 按字典类型查询数据列表
     */
    List<DictData> getDictDataListByType(@Param("dictType") String dictType);
}
