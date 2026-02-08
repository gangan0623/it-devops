

package net.leoch.modules.sys.dao;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.sys.entity.SysMenuEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 菜单管理
 * 
 * @author Taohongqiang
 */
@Mapper
public interface SysMenuDao extends BaseMapper<SysMenuEntity> {

	SysMenuEntity getById(@Param("id") Long id);

	/**
	 * 查询所有菜单列表
	 *
	 * @param menuType 菜单类型
	 */
	List<SysMenuEntity> getMenuList(@Param("menuType") Integer menuType);

	/**
	 * 查询用户菜单列表
	 *
	 * @param userId 用户ＩＤ
	 * @param menuType 菜单类型
	 */
	List<SysMenuEntity> getUserMenuList(@Param("userId") Long userId, @Param("menuType") Integer menuType);

	/**
	 * 查询用户权限列表
	 * @param userId  用户ID
	 */
	List<String> getUserPermissionsList(Long userId);

	/**
	 * 查询所有权限列表
	 */
	List<String> getPermissionsList();

	/**
	 * 根据父菜单，查询子菜单
	 * @param pid  父菜单ID
	 */
	List<SysMenuEntity> getListPid(Long pid);

}
