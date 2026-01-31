package net.leoch.modules.ops.service;

import jakarta.servlet.http.HttpServletResponse;
import net.leoch.common.page.PageData;
import net.leoch.common.service.CrudService;
import net.leoch.modules.ops.dto.*;
import net.leoch.modules.ops.entity.DeviceBackupEntity;

/**
 * 设备备份表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
public interface DeviceBackupService extends CrudService<DeviceBackupEntity, DeviceBackupDTO> {

    PageData<DeviceBackupDTO> page(DeviceBackupPageRequest request);

    DeviceBackupDTO get(DeviceBackupIdRequest request);

    void save(DeviceBackupSaveRequest request);

    void update(DeviceBackupUpdateRequest request);

    void updateStatus(DeviceBackupStatusUpdateRequest request);

    boolean online(DeviceBackupOnlineRequest request);

    boolean check(DeviceBackupCheckRequest request);

    void importExcel(DeviceBackupImportRequest request) throws Exception;

    void template(HttpServletResponse response) throws Exception;

    void export(DeviceBackupPageRequest request, HttpServletResponse response) throws Exception;

    void delete(DeviceBackupDeleteRequest request);

    void updateStatus(Long[] ids, Integer status);

    boolean existsByInstanceOrName(String instance, String name, Long excludeId);
}
