package net.leoch.modules.ops.excel.template;

import com.alibaba.excel.annotation.ExcelProperty;
import lombok.Data;

/**
 * 设备备份导入模板
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Data
public class DeviceBackupImportExcel {
    @ExcelProperty(value = "地址")
    private String instance;
    @ExcelProperty(value = "名称")
    private String name;
    @ExcelProperty(value = "用户名")
    private String username;
    @ExcelProperty(value = "密码")
    private String password;
    @ExcelProperty(value = "区域名称")
    private String areaName;
    @ExcelProperty(value = "分组名称")
    private String groupName;
    @ExcelProperty(value = "设备型号")
    private String deviceModel;
    @ExcelProperty(value = "状态(0禁用,1启用)")
    private Integer status;
    @ExcelProperty(value = "节点ID")
    private Long agentId;
}
