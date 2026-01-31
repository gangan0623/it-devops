

package net.leoch.modules.job.service;

import net.leoch.common.page.PageData;
import net.leoch.common.service.BaseService;
import net.leoch.modules.job.dto.ScheduleJobLogDTO;
import net.leoch.modules.job.entity.ScheduleJobLogEntity;

import java.util.Map;

/**
 * 定时任务日志
 *
 * @author Taohongqiang
 */
public interface ScheduleJobLogService extends BaseService<ScheduleJobLogEntity> {

	PageData<ScheduleJobLogDTO> page(Map<String, Object> params);

	ScheduleJobLogDTO get(Long id);
}
