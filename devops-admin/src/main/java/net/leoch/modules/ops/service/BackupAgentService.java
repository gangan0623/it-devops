package net.leoch.modules.ops.service;

import jakarta.servlet.http.HttpServletResponse;
import net.leoch.common.page.PageData;
import net.leoch.common.service.CrudService;
import net.leoch.modules.ops.dto.*;
import net.leoch.modules.ops.entity.BackupAgentEntity;

/**
 * 备份节点表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
public interface BackupAgentService extends CrudService<BackupAgentEntity, BackupAgentDTO> {

    PageData<BackupAgentDTO> page(BackupAgentPageRequest request);

    BackupAgentDTO get(BackupAgentIdRequest request);

    void save(BackupAgentSaveRequest request);

    void update(BackupAgentUpdateRequest request);

    void updateStatus(BackupAgentStatusUpdateRequest request);

    boolean online(BackupAgentOnlineRequest request);

    boolean check(BackupAgentCheckRequest request);

    void importExcel(BackupAgentImportRequest request) throws Exception;

    void template(HttpServletResponse response) throws Exception;

    void export(BackupAgentPageRequest request, HttpServletResponse response) throws Exception;

    void delete(BackupAgentDeleteRequest request);

    void updateStatus(Long[] ids, Integer status);

    boolean existsByInstanceOrName(String instance, String name, Long excludeId);

    OpsHostStatusSummaryDTO summary(BackupAgentPageRequest request);
}
