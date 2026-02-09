package net.leoch.modules.ops.service;

import com.baomidou.mybatisplus.extension.service.IService;
import jakarta.servlet.http.HttpServletResponse;
import net.leoch.common.data.page.PageData;
import net.leoch.modules.ops.vo.req.*;
import net.leoch.modules.ops.vo.rsp.*;
import net.leoch.modules.ops.entity.BackupAgentEntity;

/**
 * 备份节点表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
public interface IBackupAgentService extends IService<BackupAgentEntity> {

    PageData<BackupAgentRsp> page(BackupAgentPageReq request);

    BackupAgentRsp get(BackupAgentIdReq request);

    void save(BackupAgentSaveReq request);

    void update(BackupAgentUpdateReq request);

    void updateStatus(BackupAgentStatusUpdateReq request);

    boolean online(BackupAgentOnlineReq request);

    boolean check(BackupAgentCheckReq request);

    void importExcel(BackupAgentImportReq request) throws Exception;

    void template(HttpServletResponse response) throws Exception;

    void export(BackupAgentPageReq request, HttpServletResponse response) throws Exception;

    void delete(BackupAgentDeleteReq request);

    void updateStatus(Long[] ids, Integer status);

    boolean existsByInstanceOrName(String instance, String name, Long excludeId);

    OpsHostStatusSummaryRsp summary(BackupAgentPageReq request);
}
