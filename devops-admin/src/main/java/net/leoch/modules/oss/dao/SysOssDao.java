

package net.leoch.modules.oss.dao;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.oss.entity.SysOssEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * 文件上传
 * 
 * @author Taohongqiang
 */
@Mapper
public interface SysOssDao extends BaseMapper<SysOssEntity> {
	
}
