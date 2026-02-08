

package net.leoch.modules.sys.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * 系统数据
 *
 * @author Taohongqiang
 * @since 1.0.0
 */
@Data
@Schema(title = "系统数据")
public class SystemRsp implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    private Long sysTime;
    private String osName;
    private String osArch;
    private String osVersion;
    private String userLanguage;
    private String userDir;
    private Long totalPhysical;
    private Long freePhysical;
    private BigDecimal memoryRate;
    private Integer processors;
    private String jvmName;
    private String javaVersion;
    private String javaHome;
    private Long javaTotalMemory;
    private Long javaFreeMemory;
    private Long javaMaxMemory;
    private String userName;
    private BigDecimal systemCpuLoad;
    private String userTimezone;

}
