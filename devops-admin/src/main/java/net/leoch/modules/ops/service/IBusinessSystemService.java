package net.leoch.modules.ops.service;

import com.baomidou.mybatisplus.extension.service.IService;
import jakarta.servlet.http.HttpServletResponse;
import net.leoch.common.page.PageData;
import net.leoch.modules.ops.vo.req.*;
import net.leoch.modules.ops.vo.rsp.*;
import net.leoch.modules.ops.entity.BusinessSystemEntity;

/**
 * 业务系统表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
public interface IBusinessSystemService extends IService<BusinessSystemEntity> {

    PageData<BusinessSystemRsp> page(BusinessSystemPageReq request);

    BusinessSystemRsp get(BusinessSystemIdReq request);

    void save(BusinessSystemSaveReq dto);

    void update(BusinessSystemUpdateReq dto);

    void updateStatus(BusinessSystemStatusUpdateReq request);

    boolean online(BusinessSystemOnlineReq request);

    boolean check(BusinessSystemCheckReq request);

    void importExcel(BusinessSystemImportReq request) throws Exception;

    void template(HttpServletResponse response) throws Exception;

    void export(BusinessSystemPageReq request, HttpServletResponse response) throws Exception;

    void delete(BusinessSystemDeleteReq request);

    void updateStatus(Long[] ids, Integer status);

    boolean existsByInstanceOrName(String instance, String name, Long excludeId);

    OpsHostStatusSummaryRsp summary(BusinessSystemPageReq request);
}
