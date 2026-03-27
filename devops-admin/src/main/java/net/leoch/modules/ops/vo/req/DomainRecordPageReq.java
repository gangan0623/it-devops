package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.data.page.BasePage;

import java.util.Date;

/**
 * 域名记录分页请求
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(name = "DomainRecordPageReq")
public class DomainRecordPageReq extends BasePage {

    @Schema(description = "项目名称")
    private String projectName;

    @Schema(description = "域名")
    private String domainName;

    @Schema(description = "区域名称")
    private String areaName;

    @Schema(description = "分组名称")
    private String groupName;

    @Schema(description = "站点位置")
    private String siteLocation;

    @Schema(description = "是否走应用交付 0否 1是")
    private Integer adEnabled;

    @Schema(description = "是否启用外网解析 0否 1是")
    private Integer externalEnabled;

    @Schema(description = "状态 0禁用 1启用")
    private Integer status;

    @Schema(description = "在线状态 0不在线 1在线")
    private Integer onlineStatus;

    @Schema(description = "项目负责人")
    private String projectOwner;

    @Schema(description = "申请时间开始")
    private Date applyTimeStart;

    @Schema(description = "申请时间结束")
    private Date applyTimeEnd;
}
