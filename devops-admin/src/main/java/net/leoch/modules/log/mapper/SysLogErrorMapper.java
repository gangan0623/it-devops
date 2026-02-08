

package net.leoch.modules.log.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.log.entity.SysLogErrorEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * 异常日志
 *
 * @author Taohongqiang
 * @since 1.0.0
 */
@Mapper
public interface SysLogErrorMapper extends BaseMapper<SysLogErrorEntity> {
	
}
