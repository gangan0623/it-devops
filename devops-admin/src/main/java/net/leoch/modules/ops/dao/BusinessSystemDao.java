package net.leoch.modules.ops.dao;

import net.leoch.common.dao.BaseDao;
import net.leoch.modules.ops.entity.BusinessSystemEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * 业务系统表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Mapper
public interface BusinessSystemDao extends BaseDao<BusinessSystemEntity> {
	
}