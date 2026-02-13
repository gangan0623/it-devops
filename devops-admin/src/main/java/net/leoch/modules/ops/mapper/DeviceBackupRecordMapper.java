package net.leoch.modules.ops.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.ops.entity.DeviceBackupRecordEntity;
import net.leoch.modules.ops.vo.rsp.DashboardBackupStatsRsp;
import org.apache.ibatis.annotations.Mapper;

/**
 * 设备备份信息表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-29
 */
@Mapper
public interface DeviceBackupRecordMapper extends BaseMapper<DeviceBackupRecordEntity> {

    /**
     * 获取备份统计信息（一次性聚合查询）
     * @return 备份统计结果
     */
    DashboardBackupStatsRsp getBackupStats();
}
