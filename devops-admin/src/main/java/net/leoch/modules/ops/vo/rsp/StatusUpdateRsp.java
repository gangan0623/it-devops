package net.leoch.modules.ops.vo.rsp;

import lombok.Data;

import java.io.Serializable;

/**
 * 批量状态更新
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Data
public class StatusUpdateRsp implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    private Long[] ids;
    private Integer status;
}
