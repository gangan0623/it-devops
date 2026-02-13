package net.leoch.common.utils.common;

import cn.hutool.core.util.StrUtil;
import lombok.extern.slf4j.Slf4j;

import java.time.Duration;
import java.time.Instant;
import java.util.Date;

/**
 * 时间处理工具类
 * 提供时间解析、格式化等通用方法
 *
 * @author Claude
 */
@Slf4j
public final class TimeUtils {

    private TimeUtils() {
        throw new UnsupportedOperationException("Utility class");
    }

    /**
     * 解析 ISO 8601 格式的时间字符串
     * @param value ISO 8601 格式的时间字符串（如：2024-01-01T12:00:00Z）
     * @return Date 对象，解析失败返回 null
     */
    public static Date parseDate(String value) {
        if (StrUtil.isBlank(value)) {
            return null;
        }
        try {
            return Date.from(Instant.parse(value));
        } catch (Exception e) {
            log.debug("[TimeUtils] 日期解析失败, value={}", value);
            return null;
        }
    }

    /**
     * 格式化持续时间（中文）
     * @param startsAt 开始时间
     * @param endsAt 结束时间
     * @return 中文格式的持续时间字符串（如：1小时23分45秒）
     */
    public static String formatDurationZh(Date startsAt, Date endsAt) {
        if (startsAt == null) {
            return "-";
        }
        Date end = endsAt != null ? endsAt : new Date();
        if (end.before(startsAt)) {
            return "-";
        }
        long seconds = Duration.between(startsAt.toInstant(), end.toInstant()).getSeconds();
        if (seconds < 0) {
            return "-";
        }
        if (seconds < 60) {
            return seconds + "秒";
        }
        long minutes = seconds / 60;
        long secs = seconds % 60;
        if (minutes < 60) {
            return secs > 0 ? minutes + "分" + secs + "秒" : minutes + "分";
        }
        long hours = minutes / 60;
        long mins = minutes % 60;
        if (hours < 24) {
            StringBuilder sb = new StringBuilder().append(hours).append("小时");
            if (mins > 0) {
                sb.append(mins).append("分");
            }
            if (secs > 0 && mins == 0) {
                sb.append(secs).append("秒");
            }
            return sb.toString();
        }
        long days = hours / 24;
        long hrs = hours % 24;
        StringBuilder sb = new StringBuilder().append(days).append("天");
        if (hrs > 0) {
            sb.append(hrs).append("小时");
        }
        if (mins > 0 && hrs == 0) {
            sb.append(mins).append("分");
        }
        return sb.toString();
    }
}
