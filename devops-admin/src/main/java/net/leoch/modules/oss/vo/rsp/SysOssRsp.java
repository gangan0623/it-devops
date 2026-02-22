package net.leoch.modules.oss.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;
import java.util.Date;

/**
 * 文件上传响应
 *
 * @author Taohongqiang
 */
@Data
@Schema(description = "文件上传响应")
public class SysOssRsp implements Serializable {
    private static final long serialVersionUID = 1L;

    /** ID */
    @Schema(description = "ID")
    private Long id;

    /** URL地址 */
    @Schema(description = "URL地址")
    private String url;

    /** 创建者 */
    @Schema(description = "创建者")
    private Long creator;

    /** 创建时间 */
    @Schema(description = "创建时间")
    private Date createDate;
}
