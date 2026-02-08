package net.leoch.modules.ops.dao;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.ops.entity.MonitorComponentEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * 监控组件
 */
@Mapper
public interface MonitorComponentDao extends BaseMapper<MonitorComponentEntity> {
}
