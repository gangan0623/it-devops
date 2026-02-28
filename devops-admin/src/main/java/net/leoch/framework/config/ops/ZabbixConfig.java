package net.leoch.framework.config.ops;

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
    private String name;
    private String url;
    private String username;
    private Integer status;
    private String password;
    private List<String> templates = new ArrayList<>();

}
