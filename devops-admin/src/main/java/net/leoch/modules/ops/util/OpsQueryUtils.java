package net.leoch.modules.ops.util;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.toolkit.support.SFunction;

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
}
