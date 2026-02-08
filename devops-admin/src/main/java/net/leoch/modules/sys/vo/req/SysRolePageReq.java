package net.leoch.modules.sys.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.page.BasePage;

/**
 * 角色分页请求
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(name = "SysRolePageReq")
public class SysRolePageReq extends BasePage {

    @Schema(description = "角色名")
    private String name;
}
