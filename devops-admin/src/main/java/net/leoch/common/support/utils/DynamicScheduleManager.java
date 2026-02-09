package net.leoch.common.support.utils;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.core.base.Constant;
import net.leoch.common.support.exception.ExceptionUtils;
import net.leoch.common.support.utils.SpringContextUtils;
import net.leoch.modules.job.entity.ScheduleJobEntity;
import net.leoch.modules.job.entity.ScheduleJobLogEntity;
import net.leoch.modules.job.service.IScheduleJobLogService;
import net.leoch.common.integration.schedule.task.ITask;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;
import org.springframework.scheduling.support.CronTrigger;
import org.springframework.stereotype.Component;

import java.util.Date;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledFuture;

/**
 * 动态定时任务管理器
 * <p>
 * 使用 Spring TaskScheduler 替代 Quartz，通过 ConcurrentHashMap 管理任务句柄
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class DynamicScheduleManager {

    private final ThreadPoolTaskScheduler taskScheduler;
    private final IScheduleJobLogService scheduleJobLogService;

    private static final int ERROR_MAX_LENGTH = 1900;

    private final ConcurrentHashMap<Long, ScheduledFuture<?>> futures = new ConcurrentHashMap<>();

    /**
     * 按 cron 表达式注册定时任务
     */
    public void addJob(ScheduleJobEntity job) {
        String cron = normalizeCron(job.getCronExpression());
        ScheduledFuture<?> future = taskScheduler.schedule(
                () -> executeTask(job),
                new CronTrigger(cron)
        );
        if (future != null) {
            futures.put(job.getId(), future);
        }
        log.info("[定时任务] 注册任务，任务ID: {}，beanName: {}，cron: {}", job.getId(), job.getBeanName(), cron);
    }

    /**
     * 取消并移除任务
     */
    public void removeJob(Long jobId) {
        ScheduledFuture<?> future = futures.remove(jobId);
        if (future != null) {
            future.cancel(false);
            log.info("[定时任务] 移除任务，任务ID: {}", jobId);
        }
    }

    /**
     * 暂停任务（取消调度）
     */
    public void pauseJob(Long jobId) {
        ScheduledFuture<?> future = futures.remove(jobId);
        if (future != null) {
            future.cancel(false);
            log.info("[定时任务] 暂停任务，任务ID: {}", jobId);
        }
    }

    /**
     * 恢复任务（重新注册调度）
     */
    public void resumeJob(ScheduleJobEntity job) {
        addJob(job);
        log.info("[定时任务] 恢复任务，任务ID: {}", job.getId());
    }

    /**
     * 立即执行一次
     */
    public void runOnce(ScheduleJobEntity job) {
        taskScheduler.execute(() -> executeTask(job));
        log.info("[定时任务] 立即执行，任务ID: {}", job.getId());
    }

    /**
     * 更新任务（先移除再注册）
     */
    public void updateJob(ScheduleJobEntity job) {
        removeJob(job.getId());
        addJob(job);
    }

    /**
     * 执行任务：获取 ITask Bean 并调用 run 方法，记录执行日志
     */
    private void executeTask(ScheduleJobEntity job) {
        ScheduleJobLogEntity jobLog = new ScheduleJobLogEntity();
        jobLog.setJobId(job.getId());
        jobLog.setBeanName(job.getBeanName());
        jobLog.setParams(job.getParams());
        jobLog.setCreateDate(new Date());

        long startTime = System.currentTimeMillis();

        try {
            log.info("[定时任务] 任务准备执行，任务ID: {}", job.getId());
            Object target = SpringContextUtils.getBean(job.getBeanName());
            ITask task = (ITask) target;
            task.run(job.getParams());

            long times = System.currentTimeMillis() - startTime;
            jobLog.setTimes((int) times);
            jobLog.setStatus(Constant.SUCCESS);

            log.info("[定时任务] 任务执行完毕，任务ID: {}，耗时: {} 毫秒", job.getId(), times);
        } catch (Exception e) {
            log.error("[定时任务] 任务执行失败，任务ID: {}", job.getId(), e);

            long times = System.currentTimeMillis() - startTime;
            jobLog.setTimes((int) times);
            jobLog.setStatus(Constant.FAIL);
            jobLog.setError(limitError(ExceptionUtils.getErrorStackTrace(e)));
        } finally {
            try {
                scheduleJobLogService.save(jobLog);
            } catch (Exception ex) {
                log.error("[定时任务] 任务日志保存失败，任务ID: {}", job.getId(), ex);
            }
        }
    }

    /**
     * 将 Quartz 风格的 cron 中的 '?' 替换为 '*'（Spring CronTrigger 不支持 '?'）
     */
    private String normalizeCron(String cron) {
        return cron.replace('?', '*');
    }

    private String limitError(String error) {
        if (error == null || error.length() <= ERROR_MAX_LENGTH) {
            return error;
        }
        return error.substring(0, ERROR_MAX_LENGTH) + "...(truncated)";
    }
}
