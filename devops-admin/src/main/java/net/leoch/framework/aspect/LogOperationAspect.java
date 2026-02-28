

package net.leoch.framework.aspect;

import cn.hutool.extra.servlet.JakartaServletUtil;
import cn.hutool.json.JSONUtil;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.annotation.LogOperation;
import net.leoch.common.enums.OperationStatusEnum;
import net.leoch.common.integration.security.SecurityUser;
import net.leoch.common.integration.security.UserDetail;
import net.leoch.common.utils.context.HttpContextUtils;
import net.leoch.modules.log.entity.SysLogOperationEntity;
import net.leoch.modules.log.service.ISysLogOperationService;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;

/**
 * 操作日志，切面处理类
 *
 * @author Taohongqiang
 */
@Slf4j
@Aspect
@Component
@RequiredArgsConstructor
public class LogOperationAspect {
    private final ISysLogOperationService sysLogOperationService;

    @Pointcut("@annotation(net.leoch.common.annotation.LogOperation)")
    public void logPointCut() {

    }

    @Around("logPointCut()")
    public Object around(ProceedingJoinPoint point) throws Throwable {
        long beginTime = System.currentTimeMillis();
        try {
            //执行方法
            Object result = point.proceed();

            //执行时长(毫秒)
            long time = System.currentTimeMillis() - beginTime;
            //保存日志
            saveLog(point, time, OperationStatusEnum.SUCCESS.value());

            return result;
        } catch (Exception e) {
            //执行时长(毫秒)
            long time = System.currentTimeMillis() - beginTime;
            //保存日志
            saveLog(point, time, OperationStatusEnum.FAIL.value());

            throw e;
        }
    }

    private void saveLog(ProceedingJoinPoint joinPoint, long time, Integer status) throws Exception {
        MethodSignature signature = (MethodSignature) joinPoint.getSignature();
        Method method = joinPoint.getTarget().getClass().getDeclaredMethod(signature.getName(), signature.getParameterTypes());
        LogOperation annotation = method.getAnnotation(LogOperation.class);

        SysLogOperationEntity log = new SysLogOperationEntity();
        if (annotation != null) {
            //注解上的描述
            log.setOperation(annotation.value());
        }

        //登录用户信息
        UserDetail user = SecurityUser.getUser();
        if (user != null) {
            log.setCreatorName(user.getUsername());
        }

        log.setStatus(status);
        log.setRequestTime((int) time);

        //请求相关信息
        HttpServletRequest request = HttpContextUtils.getHttpServletRequest();
        log.setIp(JakartaServletUtil.getClientIP(request));
        log.setUserAgent(request.getHeader(HttpHeaders.USER_AGENT));
        log.setRequestUri(request.getRequestURI());
        log.setRequestMethod(request.getMethod());

        //请求参数
        Object[] args = joinPoint.getArgs();
        try {
            String params;
            if (args == null || args.length == 0) {
                params = "{}";
            } else if (args.length == 1) {
                params = JSONUtil.toJsonStr(args[0]);
            } else {
                params = JSONUtil.toJsonStr(args);
            }
            log.setRequestParams(params);
        } catch (Exception e) {
            LogOperationAspect.log.warn("[操作日志] 序列化请求参数失败", e);
        }

        //保存到DB
        sysLogOperationService.save(log);
    }
}
