package net.leoch.modules.ops.service.impl;

import cn.hutool.core.util.StrUtil;
import net.leoch.common.data.page.PageData;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.function.Function;

final class OnlineStatusSupport {
    private OnlineStatusSupport() {}

    static Boolean resolveOnlineStatus(Object value) {
        if (value == null) {
            return null;
        }
        if (value instanceof Boolean) {
            return (Boolean) value;
        }
        if (value instanceof Number) {
            return ((Number) value).intValue() != 0;
        }
        String str = String.valueOf(value);
        if (StrUtil.isBlank(str)) {
            return null;
        }
        return "true".equalsIgnoreCase(str) || "1".equals(str);
    }

    static <T> void sortByOnlineStatus(List<T> list, String order, Function<T, Boolean> getter) {
        if (list == null || list.isEmpty()) {
            return;
        }
        Comparator<T> comparator = Comparator.comparingInt(item -> onlineRank(getter.apply(item)));
        if ("desc".equalsIgnoreCase(order)) {
            comparator = comparator.reversed();
        }
        list.sort(comparator);
    }

    static <T> PageData<T> buildPageData(List<T> list, String pageStr, String limitStr) {
        int page = parseInt(pageStr, 1);
        int limit = parseInt(limitStr, 10);
        if (page < 1) {
            page = 1;
        }
        if (limit < 1) {
            limit = 10;
        }
        int total = list == null ? 0 : list.size();
        int fromIndex = Math.min((page - 1) * limit, total);
        int toIndex = Math.min(fromIndex + limit, total);
        List<T> pageList = list == null ? Collections.emptyList() : list.subList(fromIndex, toIndex);
        return new PageData<>(pageList, total);
    }

    private static int onlineRank(Boolean status) {
        if (Boolean.TRUE.equals(status)) {
            return 2;
        }
        if (Boolean.FALSE.equals(status)) {
            return 1;
        }
        return 0;
    }

    private static int parseInt(String value, int defaultValue) {
        try {
            return Integer.parseInt(value);
        } catch (Exception ignore) {
            return defaultValue;
        }
    }
}
