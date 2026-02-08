package net.leoch.modules.log.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.page.BasePage;

/**
 * 操作日志分页请求
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(name = "SysLogOperationPageReq")
public class SysLogOperationPageReq extends BasePage {

    @Schema(description = "状态  0：失败    1：成功")
    private String status;
}
