package net.leoch.modules.log.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.data.page.BasePage;

/**
 * 登录日志分页请求
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(name = "SysLogLoginPageReq")
public class SysLogLoginPageReq extends BasePage {

    @Schema(description = "状态  0：失败    1：成功    2：账号已锁定")
    private String status;

    @Schema(description = "用户名")
    private String creatorName;
}
