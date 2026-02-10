package net.leoch.modules.ops.vo.req;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * Linux主机删除请求
 */
@Data
@Schema(name = "LinuxHostDeleteReq")
public class LinuxHostDeleteReq implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID数组")
    private Long[] ids;

    public static LinuxHostDeleteReq of(Long[] ids) {
        LinuxHostDeleteReq req = new LinuxHostDeleteReq();
        req.setIds(ids);
        return req;
    }
}
