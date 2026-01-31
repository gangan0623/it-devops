package net.leoch.modules.ops.dto;

import lombok.Data;

import java.io.Serializable;

/**
 * 批量状态更新
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Data
public class StatusUpdateDTO implements Serializable {
    private static final long serialVersionUID = 1L;

    private Long[] ids;
    private Integer status;
}
