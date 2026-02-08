package net.leoch.modules.ops.service;

import com.baomidou.mybatisplus.extension.service.IService;
import jakarta.servlet.http.HttpServletResponse;
import net.leoch.common.page.PageData;
import net.leoch.modules.ops.vo.req.*;
import net.leoch.modules.ops.vo.rsp.*;
import net.leoch.modules.ops.entity.DeviceBackupEntity;

/**
 * 设备备份表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
public interface IDeviceBackupService extends IService<DeviceBackupEntity> {

    PageData<DeviceBackupRsp> page(DeviceBackupPageReq request);

    DeviceBackupRsp get(DeviceBackupIdReq request);

    void save(DeviceBackupSaveReq request);

    void update(DeviceBackupUpdateReq request);

    void updateStatus(DeviceBackupStatusUpdateReq request);

    boolean online(DeviceBackupOnlineReq request);

    boolean check(DeviceBackupCheckReq request);

    void importExcel(DeviceBackupImportReq request) throws Exception;

    void template(HttpServletResponse response) throws Exception;

    void export(DeviceBackupPageReq request, HttpServletResponse response) throws Exception;

    void delete(DeviceBackupDeleteReq request);

    void updateStatus(Long[] ids, Integer status);

    boolean existsByInstanceOrName(String instance, String name, Long excludeId);

    OpsHostStatusSummaryRsp summary(DeviceBackupPageReq request);
}
