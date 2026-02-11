package net.leoch.framework.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * 在线状态探测配置属性
 *
 * @author Claude
 */
@Data
@Component
@ConfigurationProperties(prefix = "devops.online-status")
public class OnlineStatusProperties {

    /**
     * 超时配置
     */
    private Timeout timeout = new Timeout();

    /**
     * 线程池配置
     */
    private ThreadPool threadPool = new ThreadPool();

    @Data
    public static class Timeout {
        /**
         * Linux/Windows主机指标检查超时（毫秒）
         */
        private int metrics = 3000;

        /**
         * 业务系统Ping超时（毫秒）
         */
        private int ping = 2000;

        /**
         * 备份代理健康检查超时（毫秒）
         */
        private int agent = 2000;

        /**
         * 设备备份在线检测超时（毫秒）
         */
        private int device = 2000;

        /**
         * 异步任务Future等待超时（毫秒）
         */
        private int future = 2000;
    }

    @Data
    public static class ThreadPool {
        /**
         * 核心线程数
         */
        private int coreSize = 4;

        /**
         * 最大线程数
         */
        private int maxSize = 50;

        /**
         * 任务队列容量
         */
        private int queueCapacity = 100;
    }
}
