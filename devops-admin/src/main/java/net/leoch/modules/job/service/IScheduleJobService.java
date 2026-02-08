package net.leoch.modules.job.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.page.PageData;
import net.leoch.modules.job.vo.rsp.ScheduleJobRsp;
import net.leoch.modules.job.vo.req.ScheduleJobPageReq;
import net.leoch.modules.job.entity.ScheduleJobEntity;

public interface IScheduleJobService extends IService<ScheduleJobEntity> {
    PageData<ScheduleJobRsp> page(ScheduleJobPageReq request);
    ScheduleJobRsp get(Long id);
    void save(ScheduleJobRsp dto);
    void update(ScheduleJobRsp dto);
    void deleteBatch(Long[] ids);
    int updateBatch(Long[] ids, int status);
    void run(Long[] ids);
    void pause(Long[] ids);
    void resume(Long[] ids);
}
