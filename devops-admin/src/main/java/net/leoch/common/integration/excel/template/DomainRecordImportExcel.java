package net.leoch.common.integration.excel.template;

import com.alibaba.excel.annotation.ExcelProperty;
import com.alibaba.excel.annotation.write.style.ColumnWidth;
import com.alibaba.excel.annotation.write.style.ContentRowHeight;
import lombok.Data;

import java.util.Date;

/**
 * 域名记录导入模板（支持节点池多行合并单元格）
 * <p>
 * 通过 index 保证与 DomainRecordExcel 列位置一致，导出的 Excel 可直接再导入。
 *
 * 列布局（共32列）：
 *   0-10  基础信息
 *   11-13 高级策略
 *   14-21 应用交付（内网虚拟服务14-17 + 外网虚拟服务18-21）
 *   22    节点池名称
 *   23-25 节点明细
 *   26-27 解析配置
 *   28-31 防火墙映射
 */
@Data
@ContentRowHeight(18)
public class DomainRecordImportExcel {

    @ExcelProperty(value = "*项目名称", index = 0)
    @ColumnWidth(22)
    private String projectName;

    @ExcelProperty(value = "*域名", index = 1)
    @ColumnWidth(32)
    private String domainName;

    @ExcelProperty(value = "*区域名称", index = 2)
    @ColumnWidth(14)
    private String areaName;

    @ExcelProperty(value = "*分组名称", index = 3)
    @ColumnWidth(14)
    private String groupName;

    @ExcelProperty(value = "*站点位置", index = 4)
    @ColumnWidth(14)
    private String siteLocation;

    @ExcelProperty(value = "*状态(启用/禁用)", index = 5)
    @ColumnWidth(16)
    private String status;

    @ExcelProperty(value = "*项目负责人", index = 6)
    @ColumnWidth(14)
    private String projectOwner;

    @ExcelProperty(value = "*申请时间", index = 7)
    @ColumnWidth(20)
    private Date applyTime;

    @ExcelProperty(value = "访问地址", index = 8)
    @ColumnWidth(36)
    private String apiUrl;

    @ExcelProperty(value = "描述", index = 9)
    @ColumnWidth(28)
    private String description;

    @ExcelProperty(value = "备注", index = 10)
    @ColumnWidth(28)
    private String remark;

    // ── 高级策略 ──

    @ExcelProperty(value = "*走应用交付(是/否)", index = 11)
    @ColumnWidth(16)
    private String adEnabled;

    @ExcelProperty(value = "*开启内网配置(是/否)", index = 12)
    @ColumnWidth(16)
    private String internalEnabled;

    @ExcelProperty(value = "*开启公网配置(是/否)", index = 13)
    @ColumnWidth(16)
    private String externalEnabled;

    // ── 内网虚拟服务（走AD时填写）──

    @ExcelProperty(value = "内网虚拟服务名称", index = 14)
    @ColumnWidth(22)
    private String virtualServiceName;

    @ExcelProperty(value = "内网虚拟服务IP", index = 15)
    @ColumnWidth(18)
    private String virtualServiceIp;

    @ExcelProperty(value = "内网虚拟服务端口", index = 16)
    @ColumnWidth(14)
    private Integer virtualServicePort;

    @ExcelProperty(value = "内网虚拟协议(HTTP/HTTPS/TCP)", index = 17)
    @ColumnWidth(24)
    private String virtualServiceProtocol;

    // ── 外网虚拟服务（双向AD时填写）──

    @ExcelProperty(value = "外网虚拟服务名称", index = 18)
    @ColumnWidth(22)
    private String externalVirtualServiceName;

    @ExcelProperty(value = "外网虚拟服务IP", index = 19)
    @ColumnWidth(18)
    private String externalVirtualServiceIp;

    @ExcelProperty(value = "外网虚拟服务端口", index = 20)
    @ColumnWidth(14)
    private Integer externalVirtualServicePort;

    @ExcelProperty(value = "外网虚拟协议(HTTP/HTTPS/TCP)", index = 21)
    @ColumnWidth(24)
    private String externalVirtualServiceProtocol;

    // ── 节点池 ──

    @ExcelProperty(value = "节点池名称", index = 22)
    @ColumnWidth(18)
    private String poolName;

    // ── 节点明细（每节点一行，走AD时必填）──

    @ExcelProperty(value = "节点IP", index = 23)
    @ColumnWidth(18)
    private String nodeIp;

    @ExcelProperty(value = "节点端口", index = 24)
    @ColumnWidth(12)
    private Integer nodePort;

    @ExcelProperty(value = "节点排序", index = 25)
    @ColumnWidth(10)
    private Integer nodeSort;

    // ── 解析配置 ──

    @ExcelProperty(value = "内网目标IP", index = 26)
    @ColumnWidth(18)
    private String internalTargetIp;

    @ExcelProperty(value = "外网记录值", index = 27)
    @ColumnWidth(18)
    private String externalRecordValue;

    // ── 防火墙映射 ──

    @ExcelProperty(value = "公网IP", index = 28)
    @ColumnWidth(18)
    private String fmPublicIp;

    @ExcelProperty(value = "外部端口", index = 29)
    @ColumnWidth(12)
    private Integer fmExternalPort;

    @ExcelProperty(value = "映射内网IP", index = 30)
    @ColumnWidth(18)
    private String fmInternalIp;

    @ExcelProperty(value = "内部端口", index = 31)
    @ColumnWidth(12)
    private Integer fmInternalPort;
}
