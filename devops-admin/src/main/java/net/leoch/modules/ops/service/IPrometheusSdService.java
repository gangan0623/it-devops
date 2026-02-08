package net.leoch.modules.ops.service;

import net.leoch.modules.ops.dto.PrometheusSdRequest;
import net.leoch.modules.ops.dto.PrometheusSdResponse;

import java.util.List;

/**
 * Prometheus SD 服务
 */
public interface IPrometheusSdService {
    List<PrometheusSdResponse> linux(PrometheusSdRequest request);
    List<PrometheusSdResponse> windows(PrometheusSdRequest request);
    List<PrometheusSdResponse> httpProbe(PrometheusSdRequest request);
}
