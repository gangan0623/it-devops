package net.leoch.modules.ops.service;

import net.leoch.common.service.CrudService;
import net.leoch.modules.ops.dto.WindowHostDTO;
import net.leoch.modules.ops.entity.WindowHostEntity;

/**
 * Windows主机表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
public interface WindowHostService extends CrudService<WindowHostEntity, WindowHostDTO> {

    net.leoch.common.page.PageData<WindowHostDTO> page(net.leoch.modules.ops.dto.WindowHostPageRequest request);

    WindowHostDTO get(net.leoch.modules.ops.dto.WindowHostIdRequest request);

    void save(WindowHostDTO dto);

    void update(WindowHostDTO dto);

    void updateStatus(net.leoch.modules.ops.dto.WindowHostStatusUpdateRequest request);

    boolean online(net.leoch.modules.ops.dto.WindowHostOnlineRequest request);

    boolean check(net.leoch.modules.ops.dto.WindowHostCheckRequest request);

    void importExcel(net.leoch.modules.ops.dto.WindowHostImportRequest request) throws Exception;

    void template(jakarta.servlet.http.HttpServletResponse response) throws Exception;

    void export(net.leoch.modules.ops.dto.WindowHostPageRequest request, jakarta.servlet.http.HttpServletResponse response) throws Exception;

    void delete(net.leoch.modules.ops.dto.WindowHostDeleteRequest request);

    void updateStatus(Long[] ids, Integer status);

    boolean existsByInstanceOrName(String instance, String name, Long excludeId);

    OpsHostStatusSummaryDTO summary(WindowHostPageRequest request);
}
