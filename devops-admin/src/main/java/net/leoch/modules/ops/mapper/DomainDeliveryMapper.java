package net.leoch.modules.ops.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.ops.entity.DomainDeliveryEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * 域名应用交付表
 */
@Mapper
public interface DomainDeliveryMapper extends BaseMapper<DomainDeliveryEntity> {
}
