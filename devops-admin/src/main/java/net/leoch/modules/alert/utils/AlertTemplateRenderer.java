package net.leoch.modules.alert.utils;

import cn.hutool.core.util.StrUtil;

import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 告警模板渲染
 */
public final class AlertTemplateRenderer {
    private static final Pattern PLACEHOLDER = Pattern.compile("\\$\\{([^}]+)}");

    private AlertTemplateRenderer() {
    }

    public static String render(String template, Map<String, Object> context) {
        if (StrUtil.isBlank(template) || context == null) {
            return template;
        }
        Matcher matcher = PLACEHOLDER.matcher(template);
        StringBuffer buffer = new StringBuffer();
        while (matcher.find()) {
            String key = matcher.group(1);
            String value = getValueByPath(context, key);
            matcher.appendReplacement(buffer, value == null ? "" : Matcher.quoteReplacement(value));
        }
        matcher.appendTail(buffer);
        return buffer.toString();
    }

    @SuppressWarnings("unchecked")
    private static String getValueByPath(Map<String, Object> context, String path) {
        String[] parts = path.split("\\.");
        Object current = context;
        for (String part : parts) {
            if (!(current instanceof Map)) {
                return null;
            }
            current = ((Map<String, Object>) current).get(part);
            if (current == null) {
                return null;
            }
        }
        return String.valueOf(current);
    }
}
