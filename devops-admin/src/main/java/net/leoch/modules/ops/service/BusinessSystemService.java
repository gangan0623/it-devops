package net.leoch.modules.ops.service;

import net.leoch.common.service.CrudService;
import net.leoch.modules.ops.dto.BusinessSystemDTO;
import net.leoch.modules.ops.entity.BusinessSystemEntity;

/**
 * 业务系统表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
public interface BusinessSystemService extends CrudService<BusinessSystemEntity, BusinessSystemDTO> {

    net.leoch.common.page.PageData<BusinessSystemDTO> page(net.leoch.modules.ops.dto.BusinessSystemPageRequest request);

    BusinessSystemDTO get(net.leoch.modules.ops.dto.BusinessSystemIdRequest request);

    void save(BusinessSystemDTO dto);

    void update(BusinessSystemDTO dto);

    void updateStatus(net.leoch.modules.ops.dto.BusinessSystemStatusUpdateRequest request);

    boolean online(net.leoch.modules.ops.dto.BusinessSystemOnlineRequest request);

    boolean check(net.leoch.modules.ops.dto.BusinessSystemCheckRequest request);

    void importExcel(net.leoch.modules.ops.dto.BusinessSystemImportRequest request) throws Exception;

    void template(jakarta.servlet.http.HttpServletResponse response) throws Exception;

    void export(net.leoch.modules.ops.dto.BusinessSystemPageRequest request, jakarta.servlet.http.HttpServletResponse response) throws Exception;

    void delete(net.leoch.modules.ops.dto.BusinessSystemDeleteRequest request);

    void updateStatus(Long[] ids, Integer status);

    boolean existsByInstanceOrName(String instance, String name, Long excludeId);
}
