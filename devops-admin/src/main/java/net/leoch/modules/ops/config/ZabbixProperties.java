package net.leoch.modules.ops.config;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

/**
 * Zabbix 配置
 */
@Setter
@Getter
@Component
@ConfigurationProperties(prefix = "zabbix")
public class ZabbixProperties {
    private String url;
    private String username;
    private String password;
    private List<String> templates = new ArrayList<>();

}
