

package net.leoch.modules.sys.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.sys.entity.SysUserEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Map;

/**
 * 系统用户
 * 
 * @author Taohongqiang
 */
@Mapper
public interface SysUserMapper extends BaseMapper<SysUserEntity> {

	List<SysUserEntity> getList(Map<String, Object> params);

	SysUserEntity getById(Long id);

	SysUserEntity getByUsername(String username);

	int updatePassword(@Param("id") Long id, @Param("newPassword") String newPassword);

	/**
	 * 根据部门ID，查询用户数
	 */
	int getCountByDeptId(Long deptId);

	/**
	 * 根据部门ID,查询用户ID列表
	 */
	List<Long> getUserIdListByDeptId(List<Long> deptIdList);
}