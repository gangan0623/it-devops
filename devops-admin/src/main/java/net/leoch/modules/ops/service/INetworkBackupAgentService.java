package net.leoch.modules.ops.service;

import com.baomidou.mybatisplus.extension.service.IService;
import jakarta.servlet.http.HttpServletResponse;
import net.leoch.common.data.page.PageData;
import net.leoch.modules.ops.entity.NetworkBackupAgentEntity;
import net.leoch.modules.ops.vo.req.*;
import net.leoch.modules.ops.vo.rsp.NetworkBackupAgentRsp;
import net.leoch.modules.ops.vo.rsp.OpsHostStatusSummaryRsp;

/**
 * 备份节点表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
public interface INetworkBackupAgentService extends IService<NetworkBackupAgentEntity> {

    PageData<NetworkBackupAgentRsp> page(NetworkBackupAgentPageReq request);

    NetworkBackupAgentRsp get(NetworkBackupAgentIdReq request);

    void save(NetworkBackupAgentSaveReq request);

    void update(NetworkBackupAgentUpdateReq request);

    void updateStatus(NetworkBackupAgentStatusUpdateReq request);

    boolean online(NetworkBackupAgentOnlineReq request);

    boolean check(NetworkBackupAgentCheckReq request);

    void importExcel(NetworkBackupAgentImportReq request) throws Exception;

    void template(HttpServletResponse response) throws Exception;

    void export(NetworkBackupAgentPageReq request, HttpServletResponse response) throws Exception;

    void delete(NetworkBackupAgentDeleteReq request);

    void updateStatus(Long[] ids, Integer status);

    boolean existsByInstanceOrName(String instance, String name, Long excludeId);

    OpsHostStatusSummaryRsp summary(NetworkBackupAgentPageReq request);
}
