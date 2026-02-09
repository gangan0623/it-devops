package net.leoch.common.web.init;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.core.base.Constant;
import net.leoch.modules.job.mapper.ScheduleJobMapper;
import net.leoch.modules.job.entity.ScheduleJobEntity;
import net.leoch.common.support.utils.DynamicScheduleManager;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * 初始化定时任务数据
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class JobCommandLineRunner implements CommandLineRunner {

    private final ScheduleJobMapper scheduleJobMapper;
    private final DynamicScheduleManager scheduleManager;

    @Override
    public void run(String... args) {
        List<ScheduleJobEntity> scheduleJobList = scheduleJobMapper.selectList(null);
        for (ScheduleJobEntity scheduleJob : scheduleJobList) {
            if (scheduleJob.getStatus() == Constant.ScheduleStatus.NORMAL.getValue()) {
                scheduleManager.addJob(scheduleJob);
            }
        }
        log.info("[定时任务] 初始化完成，共加载 {} 个任务", scheduleJobList.size());
    }
}
