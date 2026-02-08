package net.leoch.modules.ops.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.ops.entity.LinuxHostEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * Linux主机表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Mapper
public interface LinuxHostMapper extends BaseMapper<LinuxHostEntity> {
	
}