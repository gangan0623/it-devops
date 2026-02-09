package net.leoch.modules.ops.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.data.page.PageData;
import net.leoch.modules.ops.vo.req.*;
import net.leoch.modules.ops.vo.rsp.*;
import net.leoch.modules.ops.entity.MonitorComponentEntity;

import java.util.List;

/**
 * 监控组件
 */
public interface IMonitorComponentService extends IService<MonitorComponentEntity> {
    PageData<MonitorComponentRsp> page(MonitorComponentPageReq request);

    MonitorComponentRsp get(MonitorComponentIdReq request);

    void save(MonitorComponentSaveReq request);

    void update(MonitorComponentUpdateReq request);

    void delete(MonitorComponentDeleteReq request);

    boolean check(MonitorComponentCheckReq request);

    boolean probe(MonitorComponentProbeReq request);

    MonitorComponentRsp versionCheck(MonitorComponentVersionReq request);

    List<MonitorComponentRsp> list(MonitorComponentListReq request);

    boolean existsByIpPortOrName(String ip, Integer port, String name, Long excludeId);
}
