

package net.leoch.modules.job.task;

import net.leoch.modules.ops.job.DeviceBackupJobService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * 设备备份定时任务
 *
 * deviceBackupTask 为spring bean名称
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-29
 */
@Component("deviceBackupTask")
public class DeviceBackupTask implements ITask {
    private final Logger logger = LoggerFactory.getLogger(getClass());
    private final DeviceBackupJobService deviceBackupJobService;

    public DeviceBackupTask(DeviceBackupJobService deviceBackupJobService) {
        this.deviceBackupJobService = deviceBackupJobService;
    }

    @Override
    public void run(String params) {
        logger.info("设备备份定时任务开始执行，参数为：{}", params);
        deviceBackupJobService.backup(params);
    }
}
