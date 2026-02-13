package net.leoch.common.utils.common;

import cn.hutool.core.util.StrUtil;
import lombok.extern.slf4j.Slf4j;

/**
 * 类型转换工具类
 * 提供安全的类型转换方法，避免代码重复
 *
 * @author Claude
 */
@Slf4j
public final class ParseUtils {

    private ParseUtils() {
        throw new UnsupportedOperationException("Utility class");
    }

    /**
     * 将对象转换为字符串
     * @param value 待转换的对象
     * @return 字符串，如果 value 为 null 则返回 null
     */
    public static String toStr(Object value) {
        return value == null ? null : String.valueOf(value);
    }

    /**
     * 将对象转换为字符串，带默认值
     * @param value 待转换的对象
     * @param fallback 默认值
     * @return 字符串，如果 value 为 null 则返回 fallback
     */
    public static String toStr(Object value, String fallback) {
        return value == null ? fallback : String.valueOf(value);
    }

    /**
     * 将对象转换为 Long
     * @param value 待转换的对象
     * @return Long 值，转换失败返回 null
     */
    public static Long toLong(Object value) {
        if (value == null) {
            return null;
        }
        try {
            if (value instanceof Long) {
                return (Long) value;
            }
            if (value instanceof Number) {
                return ((Number) value).longValue();
            }
            String str = String.valueOf(value).trim();
            if (StrUtil.isBlank(str)) {
                return null;
            }
            return Long.parseLong(str);
        } catch (NumberFormatException e) {
            log.warn("[ParseUtils] 转换Long失败, value={}, type={}", value, value.getClass().getSimpleName());
            return null;
        }
    }

    /**
     * 将对象转换为 Integer
     * @param value 待转换的对象
     * @return Integer 值，转换失败返回 null
     */
    public static Integer toInt(Object value) {
        if (value == null) {
            return null;
        }
        try {
            if (value instanceof Integer) {
                return (Integer) value;
            }
            if (value instanceof Number) {
                return ((Number) value).intValue();
            }
            String str = String.valueOf(value).trim();
            if (StrUtil.isBlank(str)) {
                return null;
            }
            return Integer.parseInt(str);
        } catch (NumberFormatException e) {
            log.warn("[ParseUtils] 转换Integer失败, value={}, type={}", value, value.getClass().getSimpleName());
            return null;
        }
    }

    /**
     * 将对象转换为 Boolean
     * @param value 待转换的对象
     * @return Boolean 值，转换失败返回 null
     */
    public static Boolean toBoolean(Object value) {
        if (value == null) {
            return null;
        }
        if (value instanceof Boolean) {
            return (Boolean) value;
        }
        String str = String.valueOf(value).trim().toLowerCase();
        if (StrUtil.isBlank(str)) {
            return null;
        }
        return "true".equals(str) || "1".equals(str) || "yes".equals(str);
    }
}
