package net.leoch.modules.ops.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.ops.entity.DeviceBackupHistoryEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * 设备备份历史表
 */
@Mapper
public interface DeviceBackupHistoryMapper extends BaseMapper<DeviceBackupHistoryEntity> {
}
