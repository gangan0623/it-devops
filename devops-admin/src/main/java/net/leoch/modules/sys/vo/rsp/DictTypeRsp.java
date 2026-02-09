package net.leoch.modules.sys.vo.rsp;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Data;

import java.util.ArrayList;
import java.util.List;

/**
 * 字典类型（聚合视图）
 *
 * @author Taohongqiang
 */
@Data
public class DictTypeRsp {
    @JsonIgnore
    private Long id;
    private String dictType;
    private List<DictDataRsp> dataList = new ArrayList<>();
}
