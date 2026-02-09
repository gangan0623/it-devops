package net.leoch.modules.ops.vo.req;

import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * 备份回调结果项
 *
 * @author Taohongqiang
 */
@Data
public class BackupCallbackItemReq implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    private String instance;
    private String url;
}
