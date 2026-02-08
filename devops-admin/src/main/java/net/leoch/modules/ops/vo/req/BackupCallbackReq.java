package net.leoch.modules.ops.vo.req;
import net.leoch.modules.ops.vo.rsp.BackupCallbackItemRsp;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;
import java.util.List;

/**
 * 备份回调请求
 */
@Data
@Schema(name = "BackupCallbackReq")
public class BackupCallbackReq implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "Agent Token")
    private String token;

    @Schema(description = "回调项")
    private List<BackupCallbackItemRsp> items;

    public static BackupCallbackReq of(String token, List<BackupCallbackItemRsp> items) {
        BackupCallbackReq request = new BackupCallbackReq();
        request.setToken(token);
        request.setItems(items);
        return request;
    }
}
