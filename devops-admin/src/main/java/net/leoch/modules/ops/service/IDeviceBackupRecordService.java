package net.leoch.modules.ops.service;

import com.baomidou.mybatisplus.extension.service.IService;
import jakarta.servlet.http.HttpServletResponse;
import net.leoch.common.page.PageData;
import net.leoch.modules.ops.dto.*;
import net.leoch.modules.ops.entity.DeviceBackupRecordEntity;

import java.util.List;

/**
 * 设备备份信息表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-29
 */
public interface IDeviceBackupRecordService extends IService<DeviceBackupRecordEntity> {
    PageData<DeviceBackupRecordDTO> page(DeviceBackupRecordPageRequest request);

    DeviceBackupRecordDTO get(DeviceBackupRecordIdRequest request);

    void delete(DeviceBackupRecordDeleteRequest request);

    List<DeviceBackupHistoryDTO> history(DeviceBackupRecordHistoryRequest request);

    List<DeviceBackupDiffLineDTO> diff(DeviceBackupRecordDiffRequest request);

    List<DeviceBackupDiffLineDTO> diffCurrent(DeviceBackupRecordDiffCurrentRequest request);

    String preview(DeviceBackupRecordPreviewRequest request);

    void download(DeviceBackupRecordDownloadRequest request, HttpServletResponse response);

    void upsertRecord(String name, String ip, String url, boolean success);

    DeviceBackupRecordDTO getByIp(String ip);
}
