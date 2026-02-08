package net.leoch.modules.job.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.page.PageData;
import net.leoch.modules.job.dto.ScheduleJobDTO;
import net.leoch.modules.job.dto.ScheduleJobPageRequest;
import net.leoch.modules.job.entity.ScheduleJobEntity;

public interface ScheduleJobService extends IService<ScheduleJobEntity> {
    PageData<ScheduleJobDTO> page(ScheduleJobPageRequest request);
    ScheduleJobDTO get(Long id);
    void save(ScheduleJobDTO dto);
    void update(ScheduleJobDTO dto);
    void deleteBatch(Long[] ids);
    int updateBatch(Long[] ids, int status);
    void run(Long[] ids);
    void pause(Long[] ids);
    void resume(Long[] ids);
}
