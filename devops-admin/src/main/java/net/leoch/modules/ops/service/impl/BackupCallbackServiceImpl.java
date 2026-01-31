package net.leoch.modules.ops.service.impl;

import net.leoch.common.utils.Result;
import net.leoch.modules.ops.dto.BackupCallbackRequest;
import net.leoch.modules.ops.job.DeviceBackupJobService;
import net.leoch.modules.ops.service.BackupCallbackService;
import org.springframework.stereotype.Service;

/**
 * 备份回调服务
 */
@Service
public class BackupCallbackServiceImpl implements BackupCallbackService {

    private final DeviceBackupJobService deviceBackupJobService;

    public BackupCallbackServiceImpl(DeviceBackupJobService deviceBackupJobService) {
        this.deviceBackupJobService = deviceBackupJobService;
    }

    @Override
    public Result<Object> callback(BackupCallbackRequest request) {
        if (request == null) {
            return new Result<>().error("无效的agent-token或回调数据为空");
        }
        boolean ok = deviceBackupJobService.handleCallback(request.getToken(), request.getItems());
        if (!ok) {
            return new Result<>().error("无效的agent-token或回调数据为空");
        }
        return new Result<>().ok(null);
    }
}
