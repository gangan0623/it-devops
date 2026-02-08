package net.leoch.modules.ops.service;

import com.baomidou.mybatisplus.extension.service.IService;
import jakarta.servlet.http.HttpServletResponse;
import net.leoch.common.page.PageData;
import net.leoch.modules.ops.vo.req.*;
import net.leoch.modules.ops.vo.rsp.*;
import net.leoch.modules.ops.entity.DeviceBackupRecordEntity;

import java.util.List;

/**
 * 设备备份信息表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-29
 */
public interface IDeviceBackupRecordService extends IService<DeviceBackupRecordEntity> {
    PageData<DeviceBackupRecordRsp> page(DeviceBackupRecordPageReq request);

    DeviceBackupRecordRsp get(DeviceBackupRecordIdReq request);

    void delete(DeviceBackupRecordDeleteReq request);

    List<DeviceBackupHistoryRsp> history(DeviceBackupRecordHistoryReq request);

    List<DeviceBackupDiffLineRsp> diff(DeviceBackupRecordDiffReq request);

    List<DeviceBackupDiffLineRsp> diffCurrent(DeviceBackupRecordDiffCurrentReq request);

    String preview(DeviceBackupRecordPreviewReq request);

    void download(DeviceBackupRecordDownloadReq request, HttpServletResponse response);

    void upsertRecord(String name, String ip, String url, boolean success);

    DeviceBackupRecordRsp getByIp(String ip);
}
