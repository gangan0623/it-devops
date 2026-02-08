package net.leoch.modules.ops.service;

import net.leoch.common.utils.Result;
import net.leoch.modules.ops.dto.BackupCallbackRequest;

/**
 * 备份回调服务
 */
public interface BackupCallbackService {
    Result<Object> callback(BackupCallbackRequest request);
}
