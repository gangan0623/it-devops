package net.leoch.modules.sys.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Null;
import lombok.Data;
import net.leoch.common.validator.group.AddGroup;
import net.leoch.common.validator.group.DefaultGroup;
import net.leoch.common.validator.group.UpdateGroup;
import org.hibernate.validator.constraints.Range;

import java.io.Serial;
import java.io.Serializable;
import java.util.List;

/**
 * 用户管理请求
 *
 * @author Taohongqiang
 */
@Data
@Schema(title = "用户管理请求")
public class SysUserReq implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(title = "id")
    @Null(message = "{id.null}", groups = AddGroup.class)
    @NotNull(message = "{id.require}", groups = UpdateGroup.class)
    private Long id;

    @Schema(title = "用户名")
    @NotBlank(message = "{sysuser.username.require}", groups = DefaultGroup.class)
    private String username;

    @Schema(title = "密码")
    @NotBlank(message = "{sysuser.password.require}", groups = AddGroup.class)
    private String password;

    @Schema(title = "姓名", requiredMode = Schema.RequiredMode.REQUIRED)
    @NotBlank(message = "{sysuser.realName.require}", groups = DefaultGroup.class)
    private String realName;

    @Schema(title = "头像")
    private String headUrl;

    @Schema(title = "性别   0：男   1：女    2：保密")
    @Range(min = 0, max = 2, message = "{sysuser.gender.range}", groups = DefaultGroup.class)
    private Integer gender;

    @Schema(title = "邮箱")
    @Email(message = "{sysuser.email.error}", groups = DefaultGroup.class)
    private String email;

    @Schema(title = "手机号")
    private String mobile;

    @Schema(title = "部门ID")
    @NotNull(message = "{sysuser.deptId.require}", groups = DefaultGroup.class)
    private Long deptId;

    @Schema(title = "状态  0：停用    1：正常")
    @Range(min = 0, max = 1, message = "{sysuser.status.range}", groups = DefaultGroup.class)
    private Integer status;

    @Schema(title = "角色ID列表")
    private List<Long> roleIdList;
}
