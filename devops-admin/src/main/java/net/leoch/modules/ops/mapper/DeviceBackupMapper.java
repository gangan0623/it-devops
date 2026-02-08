package net.leoch.modules.ops.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.ops.entity.DeviceBackupEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * 设备备份表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Mapper
public interface DeviceBackupMapper extends BaseMapper<DeviceBackupEntity> {
	
}