package net.leoch.modules.ops.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;
import java.util.List;

/**
 * 备份回调请求
 */
@Data
@Schema(name = "BackupCallbackRequest")
public class BackupCallbackRequest implements Serializable {
    private static final long serialVersionUID = 1L;

    @Schema(description = "Agent Token")
    private String token;

    @Schema(description = "回调项")
    private List<BackupCallbackItem> items;

    public static BackupCallbackRequest of(String token, List<BackupCallbackItem> items) {
        BackupCallbackRequest request = new BackupCallbackRequest();
        request.setToken(token);
        request.setItems(items);
        return request;
    }
}
