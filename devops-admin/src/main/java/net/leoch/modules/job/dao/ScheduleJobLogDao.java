

package net.leoch.modules.job.dao;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.job.entity.ScheduleJobLogEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * 定时任务日志
 *
 * @author Taohongqiang
 */
@Mapper
public interface ScheduleJobLogDao extends BaseMapper<ScheduleJobLogEntity> {
	
}
