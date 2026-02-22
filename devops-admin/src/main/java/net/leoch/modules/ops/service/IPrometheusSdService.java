package net.leoch.modules.ops.service;

import net.leoch.modules.ops.vo.req.PrometheusSdReq;
import net.leoch.modules.ops.vo.rsp.PrometheusSdRsp;

import java.util.List;

/**
 * Prometheus SD 服务
 */
public interface IPrometheusSdService {
    List<PrometheusSdRsp> linux(PrometheusSdReq request);
    List<PrometheusSdRsp> windows(PrometheusSdReq request);
    List<PrometheusSdRsp> httpProbe(PrometheusSdReq request);
}
