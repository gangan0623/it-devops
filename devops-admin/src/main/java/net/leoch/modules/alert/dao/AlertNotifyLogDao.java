package net.leoch.modules.alert.dao;

import net.leoch.common.dao.BaseDao;
import net.leoch.modules.alert.entity.AlertNotifyLogEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * 告警发送日志
 */
@Mapper
public interface AlertNotifyLogDao extends BaseDao<AlertNotifyLogEntity> {
}
