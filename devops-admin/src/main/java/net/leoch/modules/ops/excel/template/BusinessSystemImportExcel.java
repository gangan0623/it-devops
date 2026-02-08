package net.leoch.modules.ops.excel.template;

import com.alibaba.excel.annotation.ExcelProperty;
import lombok.Data;

/**
 * 业务系统导入模板
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Data
public class BusinessSystemImportExcel {
    @ExcelProperty(value = "地址")
    private String instance;
    @ExcelProperty(value = "名称")
    private String name;
    @ExcelProperty(value = "区域名称")
    private String areaName;
    @ExcelProperty(value = "站点位置")
    private String siteLocation;
    @ExcelProperty(value = "分组名称")
    private String menuName;
    @ExcelProperty(value = "子组名称")
    private String subMenuName;
    @ExcelProperty(value = "状态(0禁用,1启用)")
    private Integer status;
}
