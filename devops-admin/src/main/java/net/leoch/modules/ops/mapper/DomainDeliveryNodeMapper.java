package net.leoch.modules.ops.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.ops.entity.DomainDeliveryNodeEntity;
import net.leoch.modules.ops.vo.rsp.DomainRecordDetailRsp;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Result;
import org.apache.ibatis.annotations.Results;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

import java.util.List;

/**
 * 域名应用交付节点明细表
 */
@Mapper
public interface DomainDeliveryNodeMapper extends BaseMapper<DomainDeliveryNodeEntity> {

    /**
     * 根据应用交付ID查询节点明细
     */
    @Select("""
        SELECT
            id,
            domain_delivery_id,
            node_ip,
            node_port,
            sort,
            remark
        FROM tb_domain_delivery_node
        WHERE domain_delivery_id = #{domainDeliveryId}
        ORDER BY sort ASC, id ASC
        """)
    @Results(id = "domainDeliveryNodeRspMap", value = {
        @Result(column = "id", property = "id", id = true),
        @Result(column = "domain_delivery_id", property = "domainDeliveryId"),
        @Result(column = "node_ip", property = "nodeIp"),
        @Result(column = "node_port", property = "nodePort"),
        @Result(column = "sort", property = "sort"),
        @Result(column = "remark", property = "remark")
    })
    List<DomainRecordDetailRsp.DeliveryNodeRsp> selectByDomainDeliveryId(@Param("domainDeliveryId") Long domainDeliveryId);
}
