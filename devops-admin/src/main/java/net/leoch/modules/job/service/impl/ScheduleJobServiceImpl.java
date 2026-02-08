package net.leoch.modules.job.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.constant.Constant;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.ConvertUtils;
import net.leoch.modules.job.dao.ScheduleJobDao;
import net.leoch.modules.job.dto.ScheduleJobDTO;
import net.leoch.modules.job.dto.ScheduleJobPageRequest;
import net.leoch.modules.job.entity.ScheduleJobEntity;
import net.leoch.modules.job.service.ScheduleJobService;
import net.leoch.modules.job.utils.DynamicScheduleManager;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

@Slf4j
@Service
@AllArgsConstructor
public class ScheduleJobServiceImpl extends ServiceImpl<ScheduleJobDao, ScheduleJobEntity> implements ScheduleJobService {

    private final DynamicScheduleManager scheduleManager;

    @Override
    public PageData<ScheduleJobDTO> page(ScheduleJobPageRequest request) {
        IPage<ScheduleJobEntity> page = this.page(request.buildPage(),
            new LambdaQueryWrapper<ScheduleJobEntity>()
                .like(StrUtil.isNotBlank(request.getBeanName()), ScheduleJobEntity::getBeanName, request.getBeanName())
                .orderByDesc(ScheduleJobEntity::getCreateDate)
        );
        return new PageData<>(ConvertUtils.sourceToTarget(page.getRecords(), ScheduleJobDTO.class), page.getTotal());
    }

    @Override
    public ScheduleJobDTO get(Long id) {
        ScheduleJobEntity entity = this.getById(id);
        return ConvertUtils.sourceToTarget(entity, ScheduleJobDTO.class);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void save(ScheduleJobDTO dto) {
        ScheduleJobEntity entity = ConvertUtils.sourceToTarget(dto, ScheduleJobEntity.class);
        entity.setStatus(Constant.ScheduleStatus.NORMAL.getValue());
        this.save(entity);
        scheduleManager.addJob(entity);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void update(ScheduleJobDTO dto) {
        ScheduleJobEntity entity = ConvertUtils.sourceToTarget(dto, ScheduleJobEntity.class);
        this.updateById(entity);
        scheduleManager.updateJob(entity);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deleteBatch(Long[] ids) {
        for (Long id : ids) {
            scheduleManager.removeJob(id);
        }
        this.removeByIds(Arrays.asList(ids));
    }

    @Override
    public int updateBatch(Long[] ids, int status) {
        Map<String, Object> map = new HashMap<>(2);
        map.put("ids", ids);
        map.put("status", status);
        return this.getBaseMapper().updateBatch(map);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void run(Long[] ids) {
        for (Long id : ids) {
            scheduleManager.runOnce(this.getById(id));
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void pause(Long[] ids) {
        for (Long id : ids) {
            scheduleManager.pauseJob(id);
        }
        updateBatch(ids, Constant.ScheduleStatus.PAUSE.getValue());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void resume(Long[] ids) {
        for (Long id : ids) {
            ScheduleJobEntity entity = this.getById(id);
            scheduleManager.resumeJob(entity);
        }
        updateBatch(ids, Constant.ScheduleStatus.NORMAL.getValue());
    }
}
