package net.leoch.modules.sys.controller;

import lombok.RequiredArgsConstructor;
import net.leoch.common.data.result.Result;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.time.Duration;
import java.util.HashMap;
import java.util.Map;

/**
 * 健康检查接口
 *
 * @author Claude
 */
@RestController
@RequestMapping("/health")
@RequiredArgsConstructor
public class HealthController {

    private final JdbcTemplate jdbcTemplate;
    private final StringRedisTemplate stringRedisTemplate;

    /**
     * 基础健康检查
     *
     * @return 健康状态
     */
    @GetMapping
    public Result<Map<String, Object>> health() {
        Map<String, Object> healthStatus = new HashMap<>();
        healthStatus.put("status", "UP");
        healthStatus.put("timestamp", System.currentTimeMillis());

        // 检查数据库连接
        try {
            jdbcTemplate.queryForObject("SELECT 1", Integer.class);
            healthStatus.put("database", "UP");
        } catch (Exception e) {
            healthStatus.put("database", "DOWN");
            healthStatus.put("status", "DOWN");
        }

        // 检查Redis连接
        try {
            stringRedisTemplate.opsForValue().set("health:check", "ok", Duration.ofSeconds(10));
            String value = stringRedisTemplate.opsForValue().get("health:check");
            healthStatus.put("redis", "ok".equals(value) ? "UP" : "DOWN");
        } catch (Exception e) {
            healthStatus.put("redis", "DOWN");
            healthStatus.put("status", "DOWN");
        }

        return new Result<Map<String, Object>>().ok(healthStatus);
    }

    /**
     * 存活检查（仅检查服务是否运行）
     *
     * @return 简单状态
     */
    @GetMapping("/liveness")
    public Result<Map<String, String>> liveness() {
        Map<String, String> status = new HashMap<>();
        status.put("status", "UP");
        return new Result<Map<String, String>>().ok(status);
    }

    /**
     * 就绪检查（检查服务是否可接受请求）
     *
     * @return 就绪状态
     */
    @GetMapping("/readiness")
    public Result<Map<String, Object>> readiness() {
        Map<String, Object> readyStatus = new HashMap<>();
        boolean ready = true;

        // 检查数据库
        try {
            jdbcTemplate.queryForObject("SELECT 1", Integer.class);
            readyStatus.put("database", "ready");
        } catch (Exception e) {
            readyStatus.put("database", "not_ready");
            ready = false;
        }

        // 检查Redis
        try {
            stringRedisTemplate.hasKey("health:check");
            readyStatus.put("redis", "ready");
        } catch (Exception e) {
            readyStatus.put("redis", "not_ready");
            ready = false;
        }

        readyStatus.put("status", ready ? "READY" : "NOT_READY");
        return new Result<Map<String, Object>>().ok(readyStatus);
    }
}
