package net.leoch.framework.config.ops;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * HTTP超时配置
 *
 * @author Claude Code
 */
@Data
@Component
@ConfigurationProperties(prefix = "http.timeout")
public class HttpTimeoutConfig {
    /**
     * 连接超时时间（毫秒）
     */
    private int connectTimeout = 5000;

    /**
     * 读取超时时间（毫秒）
     */
    private int readTimeout = 10000;

    /**
     * 写入超时时间（毫秒）
     */
    private int writeTimeout = 10000;
}
