

package net.leoch.modules.log.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.log.entity.SysLogOperationEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * 操作日志
 *
 * @author Taohongqiang
 * @since 1.0.0
 */
@Mapper
public interface SysLogOperationMapper extends BaseMapper<SysLogOperationEntity> {
	
}
