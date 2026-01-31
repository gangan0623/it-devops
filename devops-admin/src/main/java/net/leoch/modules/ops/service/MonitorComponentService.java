package net.leoch.modules.ops.service;

import net.leoch.common.page.PageData;
import net.leoch.common.service.CrudService;
import net.leoch.modules.ops.dto.*;
import net.leoch.modules.ops.entity.MonitorComponentEntity;

import java.util.List;
import java.util.Map;

/**
 * 监控组件
 */
public interface MonitorComponentService extends CrudService<MonitorComponentEntity, MonitorComponentDTO> {
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

    PageData<MonitorComponentDTO> page(Map<String, Object> params);
}
