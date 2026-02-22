package net.leoch.modules.ops.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.modules.ops.entity.DeviceBackupHistoryEntity;
import net.leoch.modules.ops.vo.rsp.DeviceBackupHistoryRsp;

import java.util.List;
import java.util.Map;

/**
 * 设备备份历史表
 */
public interface IDeviceBackupHistoryService extends IService<DeviceBackupHistoryEntity> {

    void saveHistory(String name, String ip, String url, Integer status);

    List<DeviceBackupHistoryRsp> listByIp(String ip, Integer limit);

    List<Map<String, Object>> diffById(Long leftId, Long rightId);

    List<Map<String, Object>> diffByUrls(String leftUrl, String rightUrl);

    String previewByUrl(String url);

    DeviceBackupHistoryRsp get(Long id);
}
