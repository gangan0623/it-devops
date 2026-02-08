package net.leoch.modules.ops.config;

import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;
import java.util.List;

/**
 * Zabbix 配置
 */
@Setter
@Getter
public class ZabbixConfig {
    private String url;
    private String username;
    private String password;
    private List<String> templates = new ArrayList<>();

}
