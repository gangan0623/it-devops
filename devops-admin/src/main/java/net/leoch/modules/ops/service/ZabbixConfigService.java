package net.leoch.modules.ops.service;

import lombok.RequiredArgsConstructor;
import net.leoch.common.constant.Constant;
import net.leoch.modules.ops.config.ZabbixConfig;
import net.leoch.modules.sys.service.ISysParamsService;
import org.springframework.stereotype.Service;

/**
 * Zabbix 配置读取
 *
 * @author Taohongqiang
 */
@Service
@RequiredArgsConstructor
public class ZabbixConfigService {
    private final ISysParamsService sysParamsService;

    public ZabbixConfig getConfig() {
        return sysParamsService.getValueObject(Constant.ZABBIX_CONFIG_KEY, ZabbixConfig.class);
    }
}
