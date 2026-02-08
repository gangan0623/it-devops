package net.leoch.modules.ops.dao;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.ops.entity.WindowHostEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * Windows主机表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Mapper
public interface WindowHostDao extends BaseMapper<WindowHostEntity> {
	
}