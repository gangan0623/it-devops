package net.leoch.modules.ops.service.impl;

import lombok.extern.slf4j.Slf4j;
import net.leoch.common.data.result.Result;
import net.leoch.common.integration.schedule.service.DeviceBackupJobService;
import net.leoch.modules.ops.service.IBackupCallbackService;
import net.leoch.modules.ops.vo.req.BackupCallbackReq;
import org.springframework.stereotype.Service;

/**
 * 备份回调服务
 */
@Slf4j
@Service
public class BackupCallbackServiceImpl implements IBackupCallbackService {

    private final DeviceBackupJobService deviceBackupJobService;

    public BackupCallbackServiceImpl(DeviceBackupJobService deviceBackupJobService) {
        this.deviceBackupJobService = deviceBackupJobService;
    }

    @Override
    public Result<Object> callback(BackupCallbackReq request) {
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
