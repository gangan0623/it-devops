package net.leoch.modules.ops.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.ops.entity.NetworkDeviceBackupHistoryEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * 设备备份历史表
 */
@Mapper
public interface NetworkDeviceBackupHistoryMapper extends BaseMapper<NetworkDeviceBackupHistoryEntity> {
}
