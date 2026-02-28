package net.leoch.modules.ops.service;

import lombok.RequiredArgsConstructor;
import cn.hutool.json.JSONUtil;
import net.leoch.common.base.Constant;
import net.leoch.modules.sys.service.ISysParamsService;
import net.leoch.modules.sys.vo.req.SysParamsReq;
import net.leoch.framework.config.ops.ZabbixConfig;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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
        ZabbixConfig config = sysParamsService.getValueObject(Constant.ZABBIX_CONFIG_KEY, ZabbixConfig.class);
        if (config.getStatus() == null) {
            config.setStatus(1);
        }
        return config;
    }

    @Transactional(rollbackFor = Exception.class)
    public void saveConfig(ZabbixConfig config) {
        String json = JSONUtil.toJsonStr(config);
        int count = sysParamsService.updateValueByCode(Constant.ZABBIX_CONFIG_KEY, json);
        if (count > 0) {
            return;
        }
        SysParamsReq req = new SysParamsReq();
        req.setParamCode(Constant.ZABBIX_CONFIG_KEY);
        req.setParamValue(json);
        req.setRemark("Zabbix配置");
        sysParamsService.save(req);
    }
}
