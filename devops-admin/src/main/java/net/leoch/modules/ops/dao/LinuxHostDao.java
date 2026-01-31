package net.leoch.modules.ops.dao;

import net.leoch.common.dao.BaseDao;
import net.leoch.modules.ops.entity.LinuxHostEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * Linux主机表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Mapper
public interface LinuxHostDao extends BaseDao<LinuxHostEntity> {
	
}