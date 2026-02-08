package net.leoch.common.page;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.metadata.OrderItem;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.regex.Pattern;

/**
 * 分页请求基类
 */
@Data
@Schema(title = "分页请求基类")
public class BasePage implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * 合法排序字段名：仅允许字母、数字、下划线
     */
    private static final Pattern SAFE_ORDER_FIELD = Pattern.compile("^[a-zA-Z_][a-zA-Z0-9_]{0,63}$");

    @Schema(description = "当前页码，从1开始")
    private String page;

    @Schema(description = "每页显示记录数")
    private String limit;

    @Schema(description = "排序字段")
    private String orderField;

    @Schema(description = "排序方式，可选值(asc、desc)")
    private String order;

    public <T> Page<T> buildPage() {
        long curPage = 1;
        long size = 10;
        if (StrUtil.isNotBlank(page)) {
            curPage = Long.parseLong(page);
        }
        if (StrUtil.isNotBlank(limit)) {
            size = Long.parseLong(limit);
        }
        Page<T> p = new Page<>(curPage, size);
        if (StrUtil.isNotBlank(orderField) && StrUtil.isNotBlank(order)
                && SAFE_ORDER_FIELD.matcher(orderField).matches()) {
            p.addOrder("asc".equalsIgnoreCase(order)
                ? OrderItem.asc(orderField) : OrderItem.desc(orderField));
        }
        return p;
    }
}
