package net.leoch.modules.ops.dao;

import net.leoch.common.dao.BaseDao;
import net.leoch.modules.ops.entity.BackupAgentEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * 备份节点表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Mapper
public interface BackupAgentDao extends BaseDao<BackupAgentEntity> {
	
}