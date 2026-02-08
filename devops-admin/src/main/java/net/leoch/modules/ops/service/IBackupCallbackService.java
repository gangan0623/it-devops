package net.leoch.modules.ops.service;

import net.leoch.common.utils.Result;
import net.leoch.modules.ops.vo.req.BackupCallbackReq;

/**
 * 备份回调服务
 */
public interface IBackupCallbackService {
    Result<Object> callback(BackupCallbackReq request);
}
