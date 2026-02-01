

package net.leoch.common.exception;

import cn.hutool.core.map.MapUtil;
import jakarta.servlet.http.HttpServletRequest;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.utils.HttpContextUtils;
import net.leoch.common.utils.IpUtils;
import net.leoch.common.utils.JsonUtils;
import net.leoch.common.utils.Result;
import net.leoch.modules.log.entity.SysLogErrorEntity;
import net.leoch.modules.log.service.SysLogErrorService;
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
public class RenExceptionHandler {
    private final SysLogErrorService sysLogErrorService;

    /**
     * 处理自定义异常
     */
    @ExceptionHandler(RenException.class)
    public Result<Object> handleRenException(RenException ex) {
        Result result = new Result();
        result.error(ex.getCode(), ex.getMsg());

        return result;
    }

    @ExceptionHandler(DuplicateKeyException.class)
    public Result<Object> handleDuplicateKeyException(DuplicateKeyException ex) {
        Result result = new Result();
        result.error(ErrorCode.DB_RECORD_EXISTS);

        return result;
    }

    @ExceptionHandler(Exception.class)
    public Result<Object> handleException(Exception ex) {
        log.error(ex.getMessage(), ex);

        saveLog(ex);

        return new Result().error();
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
