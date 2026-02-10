package net.leoch.modules.job.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.data.page.PageData;
import cn.hutool.core.bean.BeanUtil;
import net.leoch.modules.job.mapper.ScheduleJobLogMapper;
import net.leoch.modules.job.vo.rsp.ScheduleJobLogRsp;
import net.leoch.modules.job.vo.req.ScheduleJobLogPageReq;
import net.leoch.modules.job.entity.ScheduleJobLogEntity;
import net.leoch.modules.job.service.IScheduleJobLogService;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class ScheduleJobLogServiceImpl extends ServiceImpl<ScheduleJobLogMapper, ScheduleJobLogEntity> implements IScheduleJobLogService {

    @Override
    public PageData<ScheduleJobLogRsp> page(ScheduleJobLogPageReq request) {
        IPage<ScheduleJobLogEntity> page = this.page(request.buildPage(),
            new LambdaQueryWrapper<ScheduleJobLogEntity>()
                .eq(StrUtil.isNotBlank(request.getJobId()), ScheduleJobLogEntity::getJobId, request.getJobId())
                .orderByDesc(ScheduleJobLogEntity::getCreateDate)
        );
        return new PageData<>(BeanUtil.copyProperties(page.getRecords(), ScheduleJobLogRsp.class), page.getTotal());
    }

    @Override
    public ScheduleJobLogRsp get(Long id) {
        ScheduleJobLogEntity entity = this.getById(id);
        return BeanUtil.copyProperties(entity, ScheduleJobLogRsp.class);
    }
}
