package net.leoch.modules.ops.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.ops.entity.NetworkHostEntity;
import org.apache.ibatis.annotations.Mapper;

@Mapper
public interface NetworkHostMapper extends BaseMapper<NetworkHostEntity> {
}
