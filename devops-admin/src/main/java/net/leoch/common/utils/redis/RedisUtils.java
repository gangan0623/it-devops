

package net.leoch.common.utils.redis;

import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Component;

import java.time.Duration;
import java.util.Collection;
import java.util.Map;
import java.util.Set;

/**
 * Redis工具类
 * 提供常用的Redis操作封装，支持泛型和Duration
 *
 * @author Taohongqiang
 */
@Slf4j
@Component
public class RedisUtils {
    @Resource
    private RedisTemplate<String, Object> redisTemplate;

    /**
     * 默认过期时长24小时
     */
    public static final Duration DEFAULT_EXPIRE = Duration.ofHours(24);

    // ========== String 操作 ==========

    /**
     * 设置值，使用默认过期时间（24小时）
     */
    public void set(String key, Object value) {
        set(key, value, DEFAULT_EXPIRE);
    }

    /**
     * 设置值并指定过期时间
     */
    public void set(String key, Object value, Duration expire) {
        try {
            redisTemplate.opsForValue().set(key, value);
            if (expire != null && !expire.isNegative()) {
                redisTemplate.expire(key, expire);
            }
        } catch (Exception e) {
            log.error("[Redis] 设置值失败, key={}", key, e);
        }
    }

    /**
     * 仅在key不存在时设置值
     *
     * @return true 设置成功，false key已存在
     */
    public Boolean setIfAbsent(String key, Object value, Duration expire) {
        try {
            Boolean result = redisTemplate.opsForValue().setIfAbsent(key, value, expire);
            return result != null && result;
        } catch (Exception e) {
            log.error("[Redis] setIfAbsent失败, key={}", key, e);
            return false;
        }
    }

    /**
     * 获取值
     */
    @SuppressWarnings("unchecked")
    public <T> T get(String key) {
        try {
            return (T) redisTemplate.opsForValue().get(key);
        } catch (Exception e) {
            log.error("[Redis] 获取值失败, key={}", key, e);
            return null;
        }
    }

    /**
     * 递增
     */
    public Long increment(String key) {
        return increment(key, 1L);
    }

    /**
     * 递增指定值
     */
    public Long increment(String key, long delta) {
        try {
            return redisTemplate.opsForValue().increment(key, delta);
        } catch (Exception e) {
            log.error("[Redis] 递增失败, key={}, delta={}", key, delta, e);
            return null;
        }
    }

    /**
     * 递减
     */
    public Long decrement(String key) {
        return decrement(key, 1L);
    }

    /**
     * 递减指定值
     */
    public Long decrement(String key, long delta) {
        try {
            return redisTemplate.opsForValue().decrement(key, delta);
        } catch (Exception e) {
            log.error("[Redis] 递减失败, key={}, delta={}", key, delta, e);
            return null;
        }
    }

    // ========== Key 操作 ==========

    /**
     * 判断key是否存在
     */
    public Boolean exists(String key) {
        try {
            return redisTemplate.hasKey(key);
        } catch (Exception e) {
            log.error("[Redis] 判断key存在失败, key={}", key, e);
            return false;
        }
    }

    /**
     * 删除key
     */
    public Boolean delete(String key) {
        try {
            return redisTemplate.delete(key);
        } catch (Exception e) {
            log.error("[Redis] 删除key失败, key={}", key, e);
            return false;
        }
    }

    /**
     * 批量删除key
     */
    public Long delete(Collection<String> keys) {
        try {
            return redisTemplate.delete(keys);
        } catch (Exception e) {
            log.error("[Redis] 批量删除key失败, keys={}", keys, e);
            return 0L;
        }
    }

    /**
     * 设置过期时间
     */
    public Boolean expire(String key, Duration expire) {
        try {
            return redisTemplate.expire(key, expire);
        } catch (Exception e) {
            log.error("[Redis] 设置过期时间失败, key={}, expire={}", key, expire, e);
            return false;
        }
    }

    /**
     * 获取剩余过期时间（秒）
     */
    public Long getExpire(String key) {
        try {
            return redisTemplate.getExpire(key);
        } catch (Exception e) {
            log.error("[Redis] 获取过期时间失败, key={}", key, e);
            return -2L;
        }
    }

    /**
     * 模糊查询keys
     */
    public Set<String> keys(String pattern) {
        try {
            return redisTemplate.keys(pattern);
        } catch (Exception e) {
            log.error("[Redis] 查询keys失败, pattern={}", pattern, e);
            return Set.of();
        }
    }

    // ========== Hash 操作 ==========

    /**
     * 获取Hash字段值
     */
    @SuppressWarnings("unchecked")
    public <T> T hGet(String key, String field) {
        try {
            return (T) redisTemplate.opsForHash().get(key, field);
        } catch (Exception e) {
            log.error("[Redis] Hash获取失败, key={}, field={}", key, field, e);
            return null;
        }
    }

    /**
     * 获取Hash所有字段
     */
    @SuppressWarnings("unchecked")
    public <T> Map<String, T> hGetAll(String key) {
        try {
            HashOperations<String, String, Object> hashOps = redisTemplate.opsForHash();
            return (Map<String, T>) hashOps.entries(key);
        } catch (Exception e) {
            log.error("[Redis] Hash获取所有字段失败, key={}", key, e);
            return Map.of();
        }
    }

    /**
     * 设置Hash字段值，使用默认过期时间
     */
    public void hSet(String key, String field, Object value) {
        hSet(key, field, value, DEFAULT_EXPIRE);
    }

    /**
     * 设置Hash字段值并指定过期时间
     */
    public void hSet(String key, String field, Object value, Duration expire) {
        try {
            redisTemplate.opsForHash().put(key, field, value);
            if (expire != null && !expire.isNegative()) {
                redisTemplate.expire(key, expire);
            }
        } catch (Exception e) {
            log.error("[Redis] Hash设置失败, key={}, field={}", key, field, e);
        }
    }

    /**
     * 批量设置Hash字段，使用默认过期时间
     */
    public void hPutAll(String key, Map<String, Object> map) {
        hPutAll(key, map, DEFAULT_EXPIRE);
    }

    /**
     * 批量设置Hash字段并指定过期时间
     */
    public void hPutAll(String key, Map<String, Object> map, Duration expire) {
        try {
            redisTemplate.opsForHash().putAll(key, map);
            if (expire != null && !expire.isNegative()) {
                redisTemplate.expire(key, expire);
            }
        } catch (Exception e) {
            log.error("[Redis] Hash批量设置失败, key={}", key, e);
        }
    }

    /**
     * 删除Hash字段
     */
    public Long hDel(String key, Object... fields) {
        try {
            return redisTemplate.opsForHash().delete(key, fields);
        } catch (Exception e) {
            log.error("[Redis] Hash删除字段失败, key={}", key, e);
            return 0L;
        }
    }

    /**
     * 判断Hash字段是否存在
     */
    public Boolean hExists(String key, String field) {
        try {
            return redisTemplate.opsForHash().hasKey(key, field);
        } catch (Exception e) {
            log.error("[Redis] Hash判断字段存在失败, key={}, field={}", key, field, e);
            return false;
        }
    }

    /**
     * Hash字段值递增
     */
    public Long hIncrement(String key, String field, long delta) {
        try {
            return redisTemplate.opsForHash().increment(key, field, delta);
        } catch (Exception e) {
            log.error("[Redis] Hash递增失败, key={}, field={}, delta={}", key, field, delta, e);
            return null;
        }
    }

    // ========== List 操作 ==========

    /**
     * 从左侧推入值，使用默认过期时间
     */
    public void leftPush(String key, Object value) {
        leftPush(key, value, DEFAULT_EXPIRE);
    }

    /**
     * 从左侧推入值并指定过期时间
     */
    public void leftPush(String key, Object value, Duration expire) {
        try {
            redisTemplate.opsForList().leftPush(key, value);
            if (expire != null && !expire.isNegative()) {
                redisTemplate.expire(key, expire);
            }
        } catch (Exception e) {
            log.error("[Redis] List左推失败, key={}", key, e);
        }
    }

    /**
     * 从右侧弹出值
     */
    @SuppressWarnings("unchecked")
    public <T> T rightPop(String key) {
        try {
            return (T) redisTemplate.opsForList().rightPop(key);
        } catch (Exception e) {
            log.error("[Redis] List右弹失败, key={}", key, e);
            return null;
        }
    }

    /**
     * 获取List长度
     */
    public Long listSize(String key) {
        try {
            return redisTemplate.opsForList().size(key);
        } catch (Exception e) {
            log.error("[Redis] 获取List长度失败, key={}", key, e);
            return 0L;
        }
    }

    /**
     * 获取List范围内的元素
     */
    @SuppressWarnings("unchecked")
    public <T> Collection<T> listRange(String key, long start, long end) {
        try {
            return (Collection<T>) redisTemplate.opsForList().range(key, start, end);
        } catch (Exception e) {
            log.error("[Redis] 获取List范围失败, key={}, start={}, end={}", key, start, end, e);
            return Set.of();
        }
    }

    // ========== Set 操作 ==========

    /**
     * 添加到Set
     */
    public Long sAdd(String key, Object... values) {
        try {
            return redisTemplate.opsForSet().add(key, values);
        } catch (Exception e) {
            log.error("[Redis] Set添加失败, key={}", key, e);
            return 0L;
        }
    }

    /**
     * 从Set删除
     */
    public Long sRemove(String key, Object... values) {
        try {
            return redisTemplate.opsForSet().remove(key, values);
        } catch (Exception e) {
            log.error("[Redis] Set删除失败, key={}", key, e);
            return 0L;
        }
    }

    /**
     * 判断是否是Set成员
     */
    public Boolean sIsMember(String key, Object value) {
        try {
            return redisTemplate.opsForSet().isMember(key, value);
        } catch (Exception e) {
            log.error("[Redis] Set判断成员失败, key={}", key, e);
            return false;
        }
    }

    /**
     * 获取Set所有成员
     */
    @SuppressWarnings("unchecked")
    public <T> Set<T> sMembers(String key) {
        try {
            return (Set<T>) redisTemplate.opsForSet().members(key);
        } catch (Exception e) {
            log.error("[Redis] Set获取成员失败, key={}", key, e);
            return Set.of();
        }
    }

    /**
     * 获取Set大小
     */
    public Long sSize(String key) {
        try {
            return redisTemplate.opsForSet().size(key);
        } catch (Exception e) {
            log.error("[Redis] Set获取大小失败, key={}", key, e);
            return 0L;
        }
    }
}
