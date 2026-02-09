

package net.leoch.modules.sys.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.sys.vo.rsp.DictDataRsp;
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
public interface SysDictDataMapper extends BaseMapper<SysDictDataEntity> {

    /**
     * 字典数据列表
     */
    List<DictDataRsp> getDictDataList();

    /**
     * 按字典类型查询数据列表
     */
    List<DictDataRsp> getDictDataListByType(@Param("dictType") String dictType);
}
