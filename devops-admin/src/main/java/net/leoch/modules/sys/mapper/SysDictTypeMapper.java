

package net.leoch.modules.sys.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.sys.vo.rsp.DictTypeRsp;
import net.leoch.modules.sys.entity.SysDictTypeEntity;
import org.apache.ibatis.annotations.Mapper;

import java.util.List;

/**
 * 字典类型
 *
 * @author Taohongqiang
 */
@Mapper
public interface SysDictTypeMapper extends BaseMapper<SysDictTypeEntity> {

    /**
     * 字典类型列表
     */
    List<DictTypeRsp> getDictTypeList();

}
