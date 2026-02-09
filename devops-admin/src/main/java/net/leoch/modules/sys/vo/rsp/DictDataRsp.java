package net.leoch.modules.sys.vo.rsp;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Data;

/**
 * 字典数据（聚合视图）
 *
 * @author Taohongqiang
 */
@Data
public class DictDataRsp {
    @JsonIgnore
    private Long dictTypeId;
    private String dictLabel;
    private String dictValue;
}
