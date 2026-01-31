package net.leoch.modules.ops.excel;

import com.alibaba.excel.annotation.ExcelProperty;
import lombok.Data;

import java.util.Date;

/**
 * 设备备份表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Data
public class DeviceBackupExcel {
    @ExcelProperty(value = "主键ID")
    private Long id;
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
    @ExcelProperty(value = "状态 0禁用 1启用")
    private Integer status;
    @ExcelProperty(value = "节点ID")
    private Long agentId;
    @ExcelProperty(value = "创建者")
    private Long creator;
    @ExcelProperty(value = "创建时间")
    private Date createDate;
    @ExcelProperty(value = "更新者")
    private Long updater;
    @ExcelProperty(value = "更新时间")
    private Date updateDate;

}
