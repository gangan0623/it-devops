

package net.leoch.framework.filter.xss;

import jakarta.servlet.*;
import jakarta.servlet.http.HttpServletRequest;
import net.leoch.common.utils.context.TraceIdUtils;

import java.io.IOException;

/**
 * XSS过滤 + TraceId生成
 * @author Taohongqiang
 */
public class XssFilter implements Filter {

	@Override
	public void init(FilterConfig config) {
	}

	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
            throws IOException, ServletException {
		try {
			// 生成并设置TraceId到MDC，用于日志追踪
			TraceIdUtils.generateAndSet();

			XssHttpServletRequestWrapper xssRequest = new XssHttpServletRequestWrapper(
					(HttpServletRequest) request);
			chain.doFilter(xssRequest, response);
		} finally {
			// 清除TraceId，避免线程池复用时的污染
			TraceIdUtils.clear();
		}
	}

	@Override
	public void destroy() {
	}

}
