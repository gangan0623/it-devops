package net.leoch.modules.sys.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;
import org.hibernate.validator.constraints.Range;

import java.io.Serial;
import java.io.Serializable;

/**
 * 当前用户个人信息修改请求
 */
@Data
@Schema(title = "当前用户个人信息修改请求")
public class UserProfileUpdateReq implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(title = "原密码", requiredMode = Schema.RequiredMode.REQUIRED)
    @NotBlank(message = "{sysuser.password.require}")
    private String password;

    @Schema(title = "新密码", requiredMode = Schema.RequiredMode.REQUIRED)
    @NotBlank(message = "{sysuser.password.require}")
    private String newPassword;

    @Schema(title = "真实姓名", requiredMode = Schema.RequiredMode.REQUIRED)
    @NotBlank(message = "{sysuser.realName.require}")
    private String realName;

    @Schema(title = "性别 0：男 1：女 2：保密", requiredMode = Schema.RequiredMode.REQUIRED)
    @NotNull(message = "{sysuser.gender.range}")
    @Range(min = 0, max = 2, message = "{sysuser.gender.range}")
    private Integer gender;

    @Schema(title = "邮箱", requiredMode = Schema.RequiredMode.REQUIRED)
    @NotBlank(message = "{sysuser.email.error}")
    @Email(message = "{sysuser.email.error}")
    private String email;

    @Schema(title = "手机号", requiredMode = Schema.RequiredMode.REQUIRED)
    @NotBlank(message = "手机号不能为空")
    private String mobile;
}
