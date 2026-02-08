package net.leoch.modules.ops.service;

import com.baomidou.mybatisplus.extension.service.IService;
import jakarta.servlet.http.HttpServletResponse;
import net.leoch.common.page.PageData;
import net.leoch.modules.ops.dto.*;
import net.leoch.modules.ops.entity.BusinessSystemEntity;

/**
 * 业务系统表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
public interface IBusinessSystemService extends IService<BusinessSystemEntity> {

    PageData<BusinessSystemDTO> page(BusinessSystemPageRequest request);

    BusinessSystemDTO get(BusinessSystemIdRequest request);

    void save(BusinessSystemDTO dto);

    void update(BusinessSystemDTO dto);

    void updateStatus(BusinessSystemStatusUpdateRequest request);

    boolean online(BusinessSystemOnlineRequest request);

    boolean check(BusinessSystemCheckRequest request);

    void importExcel(BusinessSystemImportRequest request) throws Exception;

    void template(HttpServletResponse response) throws Exception;

    void export(BusinessSystemPageRequest request, HttpServletResponse response) throws Exception;

    void delete(BusinessSystemDeleteRequest request);

    void updateStatus(Long[] ids, Integer status);

    boolean existsByInstanceOrName(String instance, String name, Long excludeId);

    OpsHostStatusSummaryDTO summary(BusinessSystemPageRequest request);
}
