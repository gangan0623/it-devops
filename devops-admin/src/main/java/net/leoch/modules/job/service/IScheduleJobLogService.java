package net.leoch.modules.job.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.data.page.PageData;
import net.leoch.modules.job.vo.rsp.ScheduleJobLogRsp;
import net.leoch.modules.job.vo.req.ScheduleJobLogPageReq;
import net.leoch.modules.job.entity.ScheduleJobLogEntity;

public interface IScheduleJobLogService extends IService<ScheduleJobLogEntity> {
    PageData<ScheduleJobLogRsp> page(ScheduleJobLogPageReq request);
    ScheduleJobLogRsp get(Long id);
}
