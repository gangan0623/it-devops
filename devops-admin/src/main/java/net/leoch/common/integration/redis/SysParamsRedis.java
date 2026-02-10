

package net.leoch.common.integration.redis;

import lombok.RequiredArgsConstructor;
import net.leoch.common.utils.redis.RedisKeys;
import net.leoch.common.utils.redis.RedisUtils;
import org.springframework.stereotype.Component;

/**
 * 参数管理
 *
 * @author Taohongqiang
 * @since 1.0.0
 */
@Component
@RequiredArgsConstructor
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
