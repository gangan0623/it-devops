package net.leoch.modules.ops.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.ops.entity.DomainDnsInternalEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * 域名内网解析表
 */
@Mapper
public interface DomainDnsInternalMapper extends BaseMapper<DomainDnsInternalEntity> {
}
