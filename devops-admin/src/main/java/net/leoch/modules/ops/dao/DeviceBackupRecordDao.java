package net.leoch.modules.ops.dao;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.ops.entity.DeviceBackupRecordEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * 设备备份信息表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-29
 */
@Mapper
public interface DeviceBackupRecordDao extends BaseMapper<DeviceBackupRecordEntity> {
}
