

package net.leoch.modules.sys.redis;

import lombok.AllArgsConstructor;
import net.leoch.common.redis.RedisKeys;
import net.leoch.common.redis.RedisUtils;
import org.springframework.stereotype.Component;

/**
 * 参数管理
 *
 * @author Taohongqiang
 * @since 1.0.0
 */
@Component
@AllArgsConstructor
public class SysParamsRedis {
    private final RedisUtils redisUtils;

    public void delete(Object[] paramCodes) {
        String key = RedisKeys.getSysParamsKey();
        redisUtils.hDel(key, paramCodes);
    }

    public void set(String paramCode, String paramValue) {
        if (paramValue == null) {
            return;
        }
        String key = RedisKeys.getSysParamsKey();
        redisUtils.hSet(key, paramCode, paramValue);
    }

    public String get(String paramCode) {
        String key = RedisKeys.getSysParamsKey();
        return (String) redisUtils.hGet(key, paramCode);
    }

}
