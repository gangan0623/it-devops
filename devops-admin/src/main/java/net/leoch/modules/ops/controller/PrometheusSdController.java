package net.leoch.modules.ops.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import net.leoch.modules.ops.vo.req.PrometheusSdReq;
import net.leoch.modules.ops.vo.rsp.PrometheusSdRsp;
import net.leoch.modules.ops.service.IPrometheusSdService;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * Prometheus HTTP SD
 */
@RestController
@RequestMapping("/sd")
@Tag(name = "Prometheus SD")
public class PrometheusSdController {

    private final IPrometheusSdService prometheusSdService;

    public PrometheusSdController(IPrometheusSdService prometheusSdService) {
        this.prometheusSdService = prometheusSdService;
    }

    @GetMapping("/linux/{area}/info")
    @Operation(summary = "Linux Prometheus SD")
    public List<PrometheusSdRsp> linux(@PathVariable String area) {
        PrometheusSdReq request = new PrometheusSdReq();
        request.setArea(area);
        return prometheusSdService.linux(request);
    }

    @GetMapping("/windows/{area}/info")
    @Operation(summary = "Windows Prometheus SD")
    public List<PrometheusSdRsp> windows(@PathVariable String area) {
        PrometheusSdReq request = new PrometheusSdReq();
        request.setArea(area);
        return prometheusSdService.windows(request);
    }

    @GetMapping("/probe/{area}/info")
    @Operation(summary = "HTTP Probe Prometheus SD")
    public List<PrometheusSdRsp> httpProbe(@PathVariable String area) {
        PrometheusSdReq request = new PrometheusSdReq();
        request.setArea(area);
        return prometheusSdService.httpProbe(request);
    }
}
