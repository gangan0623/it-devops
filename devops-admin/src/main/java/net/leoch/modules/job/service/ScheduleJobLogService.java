package net.leoch.modules.job.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.page.PageData;
import net.leoch.modules.job.dto.ScheduleJobLogDTO;
import net.leoch.modules.job.dto.ScheduleJobLogPageRequest;
import net.leoch.modules.job.entity.ScheduleJobLogEntity;

public interface ScheduleJobLogService extends IService<ScheduleJobLogEntity> {
    PageData<ScheduleJobLogDTO> page(ScheduleJobLogPageRequest request);
    ScheduleJobLogDTO get(Long id);
}
