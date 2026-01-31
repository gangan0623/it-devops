

package net.leoch.modules.sys.service.impl;

import cn.hutool.core.collection.CollUtil;
import net.leoch.common.service.impl.BaseServiceImpl;
import net.leoch.modules.sys.dao.SysRoleMenuDao;
import net.leoch.modules.sys.entity.SysRoleMenuEntity;
import net.leoch.modules.sys.service.SysRoleMenuService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;


/**
 * 角色与菜单对应关系
 * 
 * @author Taohongqiang
 */
@Service
public class SysRoleMenuServiceImpl extends BaseServiceImpl<SysRoleMenuDao, SysRoleMenuEntity> implements SysRoleMenuService {

	@Override
	@Transactional(rollbackFor = Exception.class)
	public void saveOrUpdate(Long roleId, List<Long> menuIdList) {
		//先删除角色菜单关系
		deleteByRoleIds(new Long[]{roleId});

		//角色没有一个菜单权限的情况
		if(CollUtil.isEmpty(menuIdList)){
			return ;
		}

		//保存角色菜单关系
		for(Long menuId : menuIdList){
			SysRoleMenuEntity sysRoleMenuEntity = new SysRoleMenuEntity();
			sysRoleMenuEntity.setMenuId(menuId);
			sysRoleMenuEntity.setRoleId(roleId);

			//保存
			insert(sysRoleMenuEntity);
		}
	}

	@Override
	public List<Long> getMenuIdList(Long roleId){
		return baseDao.getMenuIdList(roleId);
	}

	@Override
	@Transactional(rollbackFor = Exception.class)
	public void deleteByRoleIds(Long[] roleIds) {
		baseDao.deleteByRoleIds(roleIds);
	}

	@Override
	@Transactional(rollbackFor = Exception.class)
	public void deleteByMenuId(Long menuId) {
		baseDao.deleteByMenuId(menuId);
	}

}