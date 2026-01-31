package net.leoch.modules.ops.dao;

import net.leoch.common.dao.BaseDao;
import net.leoch.modules.ops.entity.MonitorComponentEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * 监控组件
 */
@Mapper
public interface MonitorComponentDao extends BaseDao<MonitorComponentEntity> {
}
