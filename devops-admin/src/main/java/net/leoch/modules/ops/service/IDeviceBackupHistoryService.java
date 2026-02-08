package net.leoch.modules.ops.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.modules.ops.dto.DeviceBackupHistoryDTO;
import net.leoch.modules.ops.entity.DeviceBackupHistoryEntity;

import java.util.List;
import java.util.Map;

/**
 * 设备备份历史表
 */
public interface IDeviceBackupHistoryService extends IService<DeviceBackupHistoryEntity> {

    void saveHistory(String name, String ip, String url, Integer status);

    List<DeviceBackupHistoryDTO> listByIp(String ip, Integer limit);

    List<Map<String, Object>> diffById(Long leftId, Long rightId);

    List<Map<String, Object>> diffByUrls(String leftUrl, String rightUrl);

    String previewByUrl(String url);

    DeviceBackupHistoryDTO get(Long id);
}
