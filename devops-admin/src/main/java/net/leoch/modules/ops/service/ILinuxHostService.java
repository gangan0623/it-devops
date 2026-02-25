package net.leoch.modules.ops.service;

import com.baomidou.mybatisplus.extension.service.IService;
import jakarta.servlet.http.HttpServletResponse;
import net.leoch.common.data.page.PageData;
import net.leoch.modules.ops.entity.LinuxHostEntity;
import net.leoch.modules.ops.vo.req.*;
import net.leoch.modules.ops.vo.rsp.LinuxHostRsp;
import net.leoch.modules.ops.vo.rsp.OpsHostStatusSummaryRsp;
import net.leoch.modules.ops.vo.rsp.OpsDeleteCascadeRsp;

/**
 * Linux主机表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
public interface ILinuxHostService extends IService<LinuxHostEntity> {

    PageData<LinuxHostRsp> page(LinuxHostPageReq request);

    LinuxHostRsp get(LinuxHostIdReq request);

    void save(LinuxHostSaveReq dto);

    void update(LinuxHostUpdateReq dto);

    void updateStatus(LinuxHostStatusUpdateReq request);

    boolean online(LinuxHostOnlineReq request);

    boolean check(LinuxHostCheckReq request);

    void importExcel(LinuxHostImportReq request) throws Exception;

    void template(HttpServletResponse response) throws Exception;

    void export(LinuxHostPageReq request, HttpServletResponse response) throws Exception;

    OpsDeleteCascadeRsp delete(LinuxHostDeleteReq request);

    void updateStatus(Long[] ids, Integer status);

    boolean existsByInstanceOrName(String instance, String name, Long excludeId);

    OpsHostStatusSummaryRsp summary(LinuxHostPageReq request);
}
