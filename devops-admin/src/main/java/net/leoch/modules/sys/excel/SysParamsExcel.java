

package net.leoch.modules.sys.excel;

import com.alibaba.excel.annotation.ExcelProperty;
import lombok.Data;

/**
 * 参数管理
 *
 * @author Taohongqiang
 * @since 1.0.0
 */
@Data
public class SysParamsExcel {
    @ExcelProperty("参数编码")
    private String paramCode;
    @ExcelProperty("参数值")
    private String paramValue;
    @ExcelProperty("备注")
    private String remark;

}