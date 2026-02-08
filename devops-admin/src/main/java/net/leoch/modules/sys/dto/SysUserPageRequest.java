package net.leoch.modules.sys.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.page.BasePage;

/**
 * 用户分页请求
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(name = "SysUserPageRequest")
public class SysUserPageRequest extends BasePage {

    @Schema(description = "用户名")
    private String username;

    @Schema(description = "性别")
    private String gender;

    @Schema(description = "部门ID")
    private String deptId;
}
