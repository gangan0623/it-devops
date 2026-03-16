package net.leoch.modules.ops.service;

import com.baomidou.mybatisplus.extension.service.IService;
import jakarta.servlet.http.HttpServletResponse;
import net.leoch.common.data.page.PageData;
import net.leoch.modules.ops.entity.NetworkDeviceBackupRecordEntity;
import net.leoch.modules.ops.vo.req.*;
import net.leoch.modules.ops.vo.rsp.NetworkDeviceBackupDiffContentRsp;
import net.leoch.modules.ops.vo.rsp.NetworkDeviceBackupHistoryRsp;
import net.leoch.modules.ops.vo.rsp.NetworkDeviceBackupRecordRsp;

import java.util.List;

/**
 * 设备备份信息表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-29
 */
public interface INetworkDeviceBackupRecordService extends IService<NetworkDeviceBackupRecordEntity> {
    PageData<NetworkDeviceBackupRecordRsp> page(NetworkDeviceBackupRecordPageReq request);

    NetworkDeviceBackupRecordRsp get(NetworkDeviceBackupRecordIdReq request);

    void delete(NetworkDeviceBackupRecordDeleteReq request);

    List<NetworkDeviceBackupHistoryRsp> history(NetworkDeviceBackupRecordHistoryReq request);

    NetworkDeviceBackupDiffContentRsp diff(NetworkDeviceBackupRecordDiffReq request);

    NetworkDeviceBackupDiffContentRsp diffCurrent(NetworkDeviceBackupRecordDiffCurrentReq request);

    String preview(NetworkDeviceBackupRecordPreviewReq request);

    void download(NetworkDeviceBackupRecordDownloadReq request, HttpServletResponse response);

    void upsertRecord(String name, String ip, String url, boolean success);

    NetworkDeviceBackupRecordRsp getByIp(String ip);
}
