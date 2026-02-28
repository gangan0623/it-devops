package net.leoch.modules.sys.vo.rsp;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

@Data
@Schema(title = "Zabbix主机群组")
public class ZabbixHostGroupRsp implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    private String groupId;
    private String name;
}
