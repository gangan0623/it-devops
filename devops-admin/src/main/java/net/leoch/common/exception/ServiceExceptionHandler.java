

package net.leoch.common.exception;

import cn.dev33.satoken.exception.SaTokenException;
import cn.hutool.core.map.MapUtil;
import jakarta.servlet.http.HttpServletRequest;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.utils.HttpContextUtils;
import net.leoch.common.utils.IpUtils;
import net.leoch.common.utils.JsonUtils;
import net.leoch.common.utils.Result;
import net.leoch.modules.log.entity.SysLogErrorEntity;
import net.leoch.modules.log.service.ISysLogErrorService;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.http.HttpHeaders;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import java.util.Map;


/**
 * 异常处理器
 *
 * @author Taohongqiang
 * @since 1.0.0
 */
@Slf4j
@RestControllerAdvice
@AllArgsConstructor
public class ServiceExceptionHandler {
    private final ISysLogErrorService sysLogErrorService;

    /**
     * 处理自定义异常
     */
    @ExceptionHandler(ServiceException.class)
    public Result<Object> handleRenException(ServiceException ex) {
        log.warn("[异常处理] 业务异常, code: {}, msg: {}", ex.getCode(), ex.getMsg());
        Result<Object> result = new Result<>();
        result.error(ex.getCode(), ex.getMsg());

        return result;
    }

    @ExceptionHandler(DuplicateKeyException.class)
    public Result<Object> handleDuplicateKeyException(DuplicateKeyException ex) {
        log.warn("[异常处理] 数据库唯一键冲突", ex);
        Result<Object> result = new Result<>();
        result.error(ErrorCode.DB_RECORD_EXISTS);

        return result;
    }

    @ExceptionHandler(SaTokenException.class)
    public Result<Object> handleSaTokenException(SaTokenException ex) {
        log.warn("[异常处理] 认证异常: {}", ex.getMessage());
        Result<Object> result = new Result<>();
        result.error(ErrorCode.UNAUTHORIZED, ex.getMessage());
        return result;
    }

    @ExceptionHandler(Exception.class)
    public Result<Object> handleException(Exception ex) {
        log.error(ex.getMessage(), ex);

        saveLog(ex);

        return new Result<>().error();
    }

    /**
     * 保存异常日志
     */
    private void saveLog(Exception ex) {
        SysLogErrorEntity log = new SysLogErrorEntity();

        //请求相关信息
        HttpServletRequest request = HttpContextUtils.getHttpServletRequest();
        log.setIp(IpUtils.getIpAddr(request));
        log.setUserAgent(request.getHeader(HttpHeaders.USER_AGENT));
        log.setRequestUri(request.getRequestURI());
        log.setRequestMethod(request.getMethod());
        Map<String, String> params = HttpContextUtils.getParameterMap(request);
        if (MapUtil.isNotEmpty(params)) {
            log.setRequestParams(JsonUtils.toJsonString(params));
        }

        //异常信息
        log.setErrorInfo(ExceptionUtils.getErrorStackTrace(ex));

        //保存
        sysLogErrorService.save(log);
    }
}
