

package net.leoch.modules.job.init;

import lombok.AllArgsConstructor;
import net.leoch.modules.job.dao.ScheduleJobDao;
import net.leoch.modules.job.entity.ScheduleJobEntity;
import net.leoch.modules.job.utils.ScheduleUtils;
import org.quartz.CronTrigger;
import org.quartz.Scheduler;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * 初始化定时任务数据
 *
 * @author Taohongqiang
 */
@Component
@AllArgsConstructor
public class JobCommandLineRunner implements CommandLineRunner {
    private final Scheduler scheduler;
    private final ScheduleJobDao scheduleJobDao;

    @Override
    public void run(String... args) {
        List<ScheduleJobEntity> scheduleJobList = scheduleJobDao.selectList(null);
        for (ScheduleJobEntity scheduleJob : scheduleJobList) {
            CronTrigger cronTrigger = ScheduleUtils.getCronTrigger(scheduler, scheduleJob.getId());
            //如果不存在，则创建
            if (cronTrigger == null) {
                ScheduleUtils.createScheduleJob(scheduler, scheduleJob);
            } else {
                ScheduleUtils.updateScheduleJob(scheduler, scheduleJob);
            }
        }
    }
}