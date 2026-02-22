package net.leoch.framework.config.ops;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * DevOps平台配置属性（在线状态探测、HTTP超时、备份、缓存等）
 *
 * @author Claude
 */
@Data
@Component
@ConfigurationProperties(prefix = "devops")
public class OnlineStatusConfig {

    /**
     * 在线状态探测配置
     */
    private OnlineStatus onlineStatus = new OnlineStatus();

    /**
     * HTTP超时配置
     */
    private Http http = new Http();

    /**
     * 备份配置
     */
    private Backup backup = new Backup();

    /**
     * 缓存配置
     */
    private Cache cache = new Cache();

    /**
     * SSE配置
     */
    private Sse sse = new Sse();

    @Data
    public static class OnlineStatus {
        /**
         * 超时配置
         */
        private Timeout timeout = new Timeout();

        /**
         * 线程池配置
         */
        private ThreadPool threadPool = new ThreadPool();
    }

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

    @Data
    public static class Http {
        /**
         * HTTP超时配置
         */
        private Timeout timeout = new Timeout();

        @Data
        public static class Timeout {
            /**
             * 监控组件健康检查连接超时（毫秒）
             */
            private int monitorHealthCheckConnect = 3000;

            /**
             * 监控组件健康检查读取超时（毫秒）
             */
            private int monitorHealthCheckRead = 3000;

            /**
             * 监控组件版本获取连接超时（毫秒）
             */
            private int monitorVersionFetchConnect = 5000;

            /**
             * 监控组件版本获取读取超时（毫秒）
             */
            private int monitorVersionFetchRead = 8000;

            /**
             * 备份代理健康检查连接超时（毫秒）
             */
            private int backupAgentHealthConnect = 2000;

            /**
             * 备份代理健康检查读取超时（毫秒）
             */
            private int backupAgentHealthRead = 2000;

            /**
             * 触发备份任务连接超时（毫秒）
             */
            private int backupTriggerConnect = 5000;

            /**
             * 触发备份任务读取超时（毫秒）
             */
            private int backupTriggerRead = 10000;
        }
    }

    @Data
    public static class Backup {
        /**
         * 备份代理默认端口
         */
        private int agentDefaultPort = 8120;

        /**
         * 备份文件下载连接超时（毫秒）
         */
        private int downloadConnectTimeout = 5000;

        /**
         * 备份文件下载读取超时（毫秒）
         */
        private int downloadReadTimeout = 15000;
    }

    @Data
    public static class Cache {
        /**
         * 在线状态缓存过期时间（秒）
         */
        private int onlineStatusTtl = 600;
    }

    @Data
    public static class Sse {
        /**
         * SSE连接超时（0表示无限制，单位：毫秒）
         */
        private long emitterTimeout = 0L;

        /**
         * SSE批量推送上限（防止单次推送过多数据）
         */
        private int batchSizeLimit = 50;
    }
}
