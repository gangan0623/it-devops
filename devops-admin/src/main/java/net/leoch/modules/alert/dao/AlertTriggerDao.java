package net.leoch.modules.alert.dao;

import net.leoch.common.dao.BaseDao;
import net.leoch.modules.alert.entity.AlertTriggerEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * 告警触发器
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Mapper
public interface AlertTriggerDao extends BaseDao<AlertTriggerEntity> {
}
