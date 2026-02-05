package net.leoch.modules.ops.service;

import net.leoch.common.page.PageData;
import net.leoch.common.service.CrudService;
import net.leoch.modules.ops.dto.*;
import net.leoch.modules.ops.entity.LinuxHostEntity;

/**
 * Linux主机表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
public interface LinuxHostService extends CrudService<LinuxHostEntity, LinuxHostDTO> {

    PageData<LinuxHostDTO> page(LinuxHostPageRequest request);

    LinuxHostDTO get(LinuxHostIdRequest request);

    void save(LinuxHostDTO dto);

    void update(LinuxHostDTO dto);

    void updateStatus(LinuxHostStatusUpdateRequest request);

    boolean online(LinuxHostOnlineRequest request);

    boolean check(LinuxHostCheckRequest request);

    void importExcel(LinuxHostImportRequest request) throws Exception;

    void template(jakarta.servlet.http.HttpServletResponse response) throws Exception;

    void export(LinuxHostPageRequest request, jakarta.servlet.http.HttpServletResponse response) throws Exception;

    void delete(LinuxHostDeleteRequest request);

    void updateStatus(Long[] ids, Integer status);

    boolean existsByInstanceOrName(String instance, String name, Long excludeId);

    OpsHostStatusSummaryDTO summary(LinuxHostPageRequest request);
}
