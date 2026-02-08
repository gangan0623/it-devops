package net.leoch.modules.ops.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.page.PageData;
import net.leoch.modules.ops.dto.*;
import net.leoch.modules.ops.entity.MonitorComponentEntity;

import java.util.List;

/**
 * 监控组件
 */
public interface IMonitorComponentService extends IService<MonitorComponentEntity> {
    PageData<MonitorComponentDTO> page(MonitorComponentPageRequest request);

    MonitorComponentDTO get(MonitorComponentIdRequest request);

    void save(MonitorComponentSaveRequest request);

    void update(MonitorComponentUpdateRequest request);

    void delete(MonitorComponentDeleteRequest request);

    boolean check(MonitorComponentCheckRequest request);

    boolean probe(MonitorComponentProbeRequest request);

    MonitorComponentDTO versionCheck(MonitorComponentVersionRequest request);

    List<MonitorComponentDTO> list(MonitorComponentListRequest request);

    boolean existsByIpPortOrName(String ip, Integer port, String name, Long excludeId);
}
