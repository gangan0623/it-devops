package net.leoch.modules.job.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.ConvertUtils;
import net.leoch.modules.job.dao.ScheduleJobLogDao;
import net.leoch.modules.job.dto.ScheduleJobLogDTO;
import net.leoch.modules.job.dto.ScheduleJobLogPageRequest;
import net.leoch.modules.job.entity.ScheduleJobLogEntity;
import net.leoch.modules.job.service.ScheduleJobLogService;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class ScheduleJobLogServiceImpl extends ServiceImpl<ScheduleJobLogDao, ScheduleJobLogEntity> implements ScheduleJobLogService {

    @Override
    public PageData<ScheduleJobLogDTO> page(ScheduleJobLogPageRequest request) {
        IPage<ScheduleJobLogEntity> page = this.page(request.buildPage(),
            new LambdaQueryWrapper<ScheduleJobLogEntity>()
                .eq(StrUtil.isNotBlank(request.getJobId()), ScheduleJobLogEntity::getJobId, request.getJobId())
                .orderByDesc(ScheduleJobLogEntity::getCreateDate)
        );
        return new PageData<>(ConvertUtils.sourceToTarget(page.getRecords(), ScheduleJobLogDTO.class), page.getTotal());
    }

    @Override
    public ScheduleJobLogDTO get(Long id) {
        ScheduleJobLogEntity entity = this.getById(id);
        return ConvertUtils.sourceToTarget(entity, ScheduleJobLogDTO.class);
    }
}
