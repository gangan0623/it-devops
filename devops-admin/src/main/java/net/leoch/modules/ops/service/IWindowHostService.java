package net.leoch.modules.ops.service;

import com.baomidou.mybatisplus.extension.service.IService;
import jakarta.servlet.http.HttpServletResponse;
import net.leoch.common.page.PageData;
import net.leoch.modules.ops.dto.*;
import net.leoch.modules.ops.entity.WindowHostEntity;

/**
 * Windows主机表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
public interface IWindowHostService extends IService<WindowHostEntity> {

    PageData<WindowHostDTO> page(WindowHostPageRequest request);

    WindowHostDTO get(WindowHostIdRequest request);

    void save(WindowHostDTO dto);

    void update(WindowHostDTO dto);

    void updateStatus(WindowHostStatusUpdateRequest request);

    boolean online(WindowHostOnlineRequest request);

    boolean check(WindowHostCheckRequest request);

    void importExcel(WindowHostImportRequest request) throws Exception;

    void template(HttpServletResponse response) throws Exception;

    void export(WindowHostPageRequest request, HttpServletResponse response) throws Exception;

    void delete(WindowHostDeleteRequest request);

    void updateStatus(Long[] ids, Integer status);

    boolean existsByInstanceOrName(String instance, String name, Long excludeId);

    OpsHostStatusSummaryDTO summary(WindowHostPageRequest request);
}
