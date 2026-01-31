

package net.leoch.modules.sys.dao;

import net.leoch.common.dao.BaseDao;
import net.leoch.modules.sys.entity.SysRoleMenuEntity;
import org.apache.ibatis.annotations.Mapper;

import java.util.List;

/**
 * 角色与菜单对应关系
 * 
 * @author Taohongqiang
 */
@Mapper
public interface SysRoleMenuDao extends BaseDao<SysRoleMenuEntity> {

	/**
	 * 根据角色ID，获取菜单ID列表
	 */
	List<Long> getMenuIdList(Long roleId);

	/**
	 * 根据角色id，删除角色菜单关系
	 * @param roleIds 角色ids
	 */
	void deleteByRoleIds(Long[] roleIds);

	/**
	 * 根据菜单id，删除角色菜单关系
	 * @param menuId 菜单id
	 */
	void deleteByMenuId(Long menuId);
}
