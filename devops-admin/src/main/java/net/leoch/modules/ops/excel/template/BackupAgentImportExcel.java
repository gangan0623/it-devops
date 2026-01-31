package net.leoch.modules.ops.excel.template;

import com.alibaba.excel.annotation.ExcelProperty;
import lombok.Data;

/**
 * 备份节点导入模板
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Data
public class BackupAgentImportExcel {
    @ExcelProperty(value = "地址")
    private String instance;
    @ExcelProperty(value = "名称")
    private String name;
    @ExcelProperty(value = "区域名称")
    private String areaName;
    @ExcelProperty(value = "Token")
    private String token;
    @ExcelProperty(value = "状态(0禁用,1启用)")
    private Integer status;
}
