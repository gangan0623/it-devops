package net.leoch.modules.ops.dao;

import net.leoch.common.dao.BaseDao;
import net.leoch.modules.ops.entity.DeviceBackupHistoryEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * 设备备份历史表
 */
@Mapper
public interface DeviceBackupHistoryDao extends BaseDao<DeviceBackupHistoryEntity> {
}
