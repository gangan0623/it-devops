

package net.leoch.common.utils.context;

import cn.hutool.core.util.StrUtil;
import cn.hutool.extra.servlet.JakartaServletUtil;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.http.HttpHeaders;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.util.HashMap;
import java.util.Map;

/**
 * Http
 *
 * @author Taohongqiang
 */
public class HttpContextUtils {

	public static HttpServletRequest getHttpServletRequest() {
		RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
		if(requestAttributes == null){
			return null;
		}

		return ((ServletRequestAttributes) requestAttributes).getRequest();
	}

    public static Map<String, String> getParameterMap(HttpServletRequest request) {
        Map<String, String> params = JakartaServletUtil.getParamMap(request);
        if (params == null || params.isEmpty()) {
            return new HashMap<>();
        }
        Map<String, String> filtered = new HashMap<>(params.size());
        for (Map.Entry<String, String> entry : params.entrySet()) {
            if (StrUtil.isNotBlank(entry.getValue())) {
                filtered.put(entry.getKey(), entry.getValue());
            }
        }
        return filtered;
    }

	public static String getDomain(){
		HttpServletRequest request = getHttpServletRequest();
		if (request == null) {
			return "";
		}
		StringBuffer url = request.getRequestURL();
		return url.delete(url.length() - request.getRequestURI().length(), url.length()).toString();
	}

	public static String getOrigin(){
		HttpServletRequest request = getHttpServletRequest();
		if (request == null) {
			return "";
		}
		return request.getHeader(HttpHeaders.ORIGIN);
	}
}
