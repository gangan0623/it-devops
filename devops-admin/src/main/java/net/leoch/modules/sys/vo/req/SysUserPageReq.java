package net.leoch.modules.sys.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.data.page.BasePage;

/**
 * 用户分页请求
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(name = "SysUserPageReq")
public class SysUserPageReq extends BasePage {

    @Schema(description = "用户名")
    private String username;

    @Schema(description = "性别")
    private String gender;

    @Schema(description = "部门ID")
    private String deptId;
}
