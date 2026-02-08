package net.leoch.modules.ops.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.ops.entity.MonitorComponentEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * 监控组件
 */
@Mapper
public interface MonitorComponentMapper extends BaseMapper<MonitorComponentEntity> {
}
