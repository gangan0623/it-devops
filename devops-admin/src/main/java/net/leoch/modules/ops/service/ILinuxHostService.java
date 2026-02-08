package net.leoch.modules.ops.service;

import com.baomidou.mybatisplus.extension.service.IService;
import jakarta.servlet.http.HttpServletResponse;
import net.leoch.common.page.PageData;
import net.leoch.modules.ops.dto.*;
import net.leoch.modules.ops.entity.LinuxHostEntity;

/**
 * Linux主机表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
public interface ILinuxHostService extends IService<LinuxHostEntity> {

    PageData<LinuxHostDTO> page(LinuxHostPageRequest request);

    LinuxHostDTO get(LinuxHostIdRequest request);

    void save(LinuxHostDTO dto);

    void update(LinuxHostDTO dto);

    void updateStatus(LinuxHostStatusUpdateRequest request);

    boolean online(LinuxHostOnlineRequest request);

    boolean check(LinuxHostCheckRequest request);

    void importExcel(LinuxHostImportRequest request) throws Exception;

    void template(HttpServletResponse response) throws Exception;

    void export(LinuxHostPageRequest request, HttpServletResponse response) throws Exception;

    void delete(LinuxHostDeleteRequest request);

    void updateStatus(Long[] ids, Integer status);

    boolean existsByInstanceOrName(String instance, String name, Long excludeId);

    OpsHostStatusSummaryDTO summary(LinuxHostPageRequest request);
}
