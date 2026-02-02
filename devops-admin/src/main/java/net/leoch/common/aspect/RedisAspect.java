

package net.leoch.common.aspect;

import net.leoch.common.exception.ErrorCode;
import net.leoch.common.exception.RenException;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * Redis切面处理类
 *
 * @author Taohongqiang
 */
@Aspect
@Component
public class RedisAspect {
    private final Logger logger = LoggerFactory.getLogger(getClass());

    @Around("execution(* net.leoch.common.redis.RedisUtils.*(..))")
    public Object around(ProceedingJoinPoint point) throws Throwable {
        Object result;
        try {
            result = point.proceed();
        } catch (Exception e) {
            logger.error("redis error", e);
            throw new RenException(ErrorCode.REDIS_ERROR);
        }
        return result;
    }
}
