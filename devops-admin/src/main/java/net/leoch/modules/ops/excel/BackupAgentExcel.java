package net.leoch.modules.ops.excel;

import com.alibaba.excel.annotation.ExcelProperty;
import lombok.Data;

import java.util.Date;

/**
 * 备份节点表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Data
public class BackupAgentExcel {
    @ExcelProperty(value = "主键ID")
    private Long id;
    @ExcelProperty(value = "地址")
    private String instance;
    @ExcelProperty(value = "名称")
    private String name;
    @ExcelProperty(value = "区域名称")
    private String areaName;
    @ExcelProperty(value = "Token")
    private String token;
    @ExcelProperty(value = "状态 0禁用 1启用")
    private Integer status;
    @ExcelProperty(value = "创建者")
    private Long creator;
    @ExcelProperty(value = "创建时间")
    private Date createDate;
    @ExcelProperty(value = "更新者")
    private Long updater;
    @ExcelProperty(value = "更新时间")
    private Date updateDate;

}
