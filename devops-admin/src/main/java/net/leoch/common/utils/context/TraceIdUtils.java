package net.leoch.common.utils.context;

import org.slf4j.MDC;

import java.util.UUID;

/**
 * TraceId工具类 - 用于分布式请求追踪
 *
 * @author Claude
 */
public class TraceIdUtils {

    private static final String TRACE_ID_KEY = "traceId";

    /**
     * 生成新的TraceId并设置到MDC
     *
     * @return 生成的traceId
     */
    public static String generateAndSet() {
        String traceId = generateTraceId();
        MDC.put(TRACE_ID_KEY, traceId);
        return traceId;
    }

    /**
     * 生成TraceId（基于UUID，去除横线）
     *
     * @return traceId字符串
     */
    private static String generateTraceId() {
        return UUID.randomUUID().toString().replace("-", "");
    }

    /**
     * 获取当前请求的TraceId
     *
     * @return traceId，若不存在返回null
     */
    public static String get() {
        return MDC.get(TRACE_ID_KEY);
    }

    /**
     * 清除当前线程的TraceId（避免线程池复用导致的污染）
     */
    public static void clear() {
        MDC.remove(TRACE_ID_KEY);
    }

    /**
     * 设置TraceId（用于跨线程传递）
     *
     * @param traceId traceId值
     */
    public static void set(String traceId) {
        if (traceId != null && !traceId.isEmpty()) {
            MDC.put(TRACE_ID_KEY, traceId);
        }
    }
}
