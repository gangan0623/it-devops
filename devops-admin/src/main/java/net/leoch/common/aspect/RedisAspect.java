

package net.leoch.common.aspect;

import lombok.extern.slf4j.Slf4j;
import net.leoch.common.exception.ErrorCode;
import net.leoch.common.exception.ServiceException;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.stereotype.Component;

/**
 * Redis切面处理类
 *
 * @author Taohongqiang
 */
@Slf4j
@Aspect
@Component
public class RedisAspect {

    @Around("execution(* net.leoch.common.redis.RedisUtils.*(..))")
    public Object around(ProceedingJoinPoint point) throws Throwable {
        Object result;
        try {
            result = point.proceed();
        } catch (Exception e) {
            log.error("[Redis] 操作异常", e);
            throw new ServiceException(ErrorCode.REDIS_ERROR);
        }
        return result;
    }
}
