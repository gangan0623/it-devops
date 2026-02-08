

package net.leoch.common.page;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;
import java.util.List;

/**
 * 分页工具类
 *
 * @author Taohongqiang
 */
@Data
@Schema(title = "分页数据")
public class PageData<T> implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(title = "总记录数")
    private int total;

    @Schema(title = "列表数据")
    private List<T> list;

    @Schema(title = "告警数量（问题页专用）")
    private int firingCount;

    @Schema(title = "恢复数量（问题页专用）")
    private int resolvedCount;

    /**
     * 分页
     * @param list   列表数据
     * @param total  总记录数
     */
    public PageData(List<T> list, long total) {
        this.list = list;
        this.total = (int)total;
    }
}
