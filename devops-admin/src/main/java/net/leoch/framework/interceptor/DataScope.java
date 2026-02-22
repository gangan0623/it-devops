

package net.leoch.framework.interceptor;

import lombok.Getter;
import lombok.Setter;

/**
 * 数据范围
 *
 * @author Taohongqiang
 * @since 1.0.0
 */
@Setter
@Getter
public class DataScope {
    private String sqlFilter;

    public DataScope(String sqlFilter) {
        this.sqlFilter = sqlFilter;
    }

    @Override
    public String toString() {
        return this.sqlFilter;
    }
}