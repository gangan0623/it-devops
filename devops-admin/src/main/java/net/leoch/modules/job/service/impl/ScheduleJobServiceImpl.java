package net.leoch.modules.job.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import lombok.AllArgsConstructor;
import net.leoch.common.constant.Constant;
import net.leoch.common.page.PageData;
import net.leoch.common.service.impl.BaseServiceImpl;
import net.leoch.common.utils.ConvertUtils;
import net.leoch.modules.job.dao.ScheduleJobDao;
import net.leoch.modules.job.dto.ScheduleJobDTO;
import net.leoch.modules.job.entity.ScheduleJobEntity;
import net.leoch.modules.job.service.ScheduleJobService;
import net.leoch.modules.job.utils.DynamicScheduleManager;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

@Service
@AllArgsConstructor
public class ScheduleJobServiceImpl extends BaseServiceImpl<ScheduleJobDao, ScheduleJobEntity> implements ScheduleJobService {

    private final DynamicScheduleManager scheduleManager;

    @Override
    public PageData<ScheduleJobDTO> page(Map<String, Object> params) {
        IPage<ScheduleJobEntity> page = baseDao.selectPage(
                getPage(params, Constant.CREATE_DATE, false),
                getWrapper(params)
        );
        return getPageData(page, ScheduleJobDTO.class);
    }

    @Override
    public ScheduleJobDTO get(Long id) {
        ScheduleJobEntity entity = baseDao.selectById(id);
        return ConvertUtils.sourceToTarget(entity, ScheduleJobDTO.class);
    }

    private QueryWrapper<ScheduleJobEntity> getWrapper(Map<String, Object> params) {
        String beanName = (String) params.get("beanName");

        QueryWrapper<ScheduleJobEntity> wrapper = new QueryWrapper<>();
        wrapper.like(StrUtil.isNotBlank(beanName), "bean_name", beanName);

        return wrapper;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void save(ScheduleJobDTO dto) {
        ScheduleJobEntity entity = ConvertUtils.sourceToTarget(dto, ScheduleJobEntity.class);
        entity.setStatus(Constant.ScheduleStatus.NORMAL.getValue());
        this.insert(entity);

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

        this.deleteBatchIds(Arrays.asList(ids));
    }

    @Override
    public int updateBatch(Long[] ids, int status) {
        Map<String, Object> map = new HashMap<>(2);
        map.put("ids", ids);
        map.put("status", status);
        return baseDao.updateBatch(map);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void run(Long[] ids) {
        for (Long id : ids) {
            scheduleManager.runOnce(this.selectById(id));
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
            ScheduleJobEntity entity = this.selectById(id);
            scheduleManager.resumeJob(entity);
        }

        updateBatch(ids, Constant.ScheduleStatus.NORMAL.getValue());
    }
}
