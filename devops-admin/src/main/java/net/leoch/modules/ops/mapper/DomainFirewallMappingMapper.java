package net.leoch.modules.ops.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.ops.entity.DomainFirewallMappingEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * 域名防火墙映射表
 */
@Mapper
public interface DomainFirewallMappingMapper extends BaseMapper<DomainFirewallMappingEntity> {
}
