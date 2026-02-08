package net.leoch.modules.ops.service;

import com.baomidou.mybatisplus.extension.service.IService;
import jakarta.servlet.http.HttpServletResponse;
import net.leoch.common.page.PageData;
import net.leoch.modules.ops.vo.req.*;
import net.leoch.modules.ops.vo.rsp.*;
import net.leoch.modules.ops.entity.WindowHostEntity;

/**
 * Windows主机表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
public interface IWindowHostService extends IService<WindowHostEntity> {

    PageData<WindowHostRsp> page(WindowHostPageReq request);

    WindowHostRsp get(WindowHostIdReq request);

    void save(WindowHostRsp dto);

    void update(WindowHostRsp dto);

    void updateStatus(WindowHostStatusUpdateReq request);

    boolean online(WindowHostOnlineReq request);

    boolean check(WindowHostCheckReq request);

    void importExcel(WindowHostImportReq request) throws Exception;

    void template(HttpServletResponse response) throws Exception;

    void export(WindowHostPageReq request, HttpServletResponse response) throws Exception;

    void delete(WindowHostDeleteReq request);

    void updateStatus(Long[] ids, Integer status);

    boolean existsByInstanceOrName(String instance, String name, Long excludeId);

    OpsHostStatusSummaryRsp summary(WindowHostPageReq request);
}
