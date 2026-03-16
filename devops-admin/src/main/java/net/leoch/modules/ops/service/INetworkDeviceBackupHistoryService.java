package net.leoch.modules.ops.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.modules.ops.entity.NetworkDeviceBackupHistoryEntity;
import net.leoch.modules.ops.vo.rsp.NetworkDeviceBackupHistoryRsp;

import java.util.List;

/**
 * 设备备份历史表
 */
public interface INetworkDeviceBackupHistoryService extends IService<NetworkDeviceBackupHistoryEntity> {

    void saveHistory(String name, String ip, String url, Integer status);

    List<NetworkDeviceBackupHistoryRsp> listByIp(String ip, Integer limit);

    String previewByUrl(String url);

    NetworkDeviceBackupHistoryRsp get(Long id);
}
