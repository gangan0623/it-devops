package net.leoch.modules.ops.util;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.OrderItem;
import com.baomidou.mybatisplus.core.toolkit.support.SFunction;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import net.leoch.common.constant.Constant;

public final class OpsQueryUtils {
    private OpsQueryUtils() {}

    public static <T> boolean existsByInstanceOrName(BaseMapper<T> mapper,
                                                     SFunction<T, ?> idGetter,
                                                     SFunction<T, String> instanceGetter,
                                                     SFunction<T, String> nameGetter,
                                                     String instance,
                                                     String name,
                                                     Long excludeId) {
        if (StrUtil.isBlank(instance) && StrUtil.isBlank(name)) {
            return false;
        }
        LambdaQueryWrapper<T> wrapper = new LambdaQueryWrapper<>();
        wrapper.and(query -> {
            if (StrUtil.isNotBlank(instance)) {
                query.or().eq(instanceGetter, instance);
            }
            if (StrUtil.isNotBlank(name)) {
                query.or().eq(nameGetter, name);
            }
        });
        if (excludeId != null) {
            wrapper.ne(idGetter, excludeId);
        }
        return mapper.selectCount(wrapper) > 0;
    }

    public static <T> Page<T> buildPage(String pageStr, String limitStr, String orderField, String order) {
        long curPage = 1;
        long limit = 10;
        if (StrUtil.isNotBlank(pageStr)) {
            curPage = Long.parseLong(pageStr);
        }
        if (StrUtil.isNotBlank(limitStr)) {
            limit = Long.parseLong(limitStr);
        }
        Page<T> page = new Page<>(curPage, limit);
        if (StrUtil.isNotBlank(orderField) && StrUtil.isNotBlank(order)) {
            if (Constant.ASC.equalsIgnoreCase(order)) {
                page.addOrder(OrderItem.asc(orderField));
            } else {
                page.addOrder(OrderItem.desc(orderField));
            }
        }
        return page;
    }
}
