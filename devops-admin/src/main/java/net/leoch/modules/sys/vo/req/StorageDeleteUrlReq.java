package net.leoch.modules.sys.vo.req;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * 按URL删除MinIO对象请求
 */
@Data
public class StorageDeleteUrlReq implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    @NotBlank(message = "url不能为空")
    private String url;
}
