package net.leoch.modules.ops.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.ops.entity.DomainDnsExternalEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * 域名外网解析表
 */
@Mapper
public interface DomainDnsExternalMapper extends BaseMapper<DomainDnsExternalEntity> {
}
