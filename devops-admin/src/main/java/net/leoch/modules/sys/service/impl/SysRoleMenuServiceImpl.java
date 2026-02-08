package net.leoch.modules.sys.service.impl;

import cn.hutool.core.collection.CollUtil;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import net.leoch.modules.sys.mapper.SysRoleMenuMapper;
import net.leoch.modules.sys.entity.SysRoleMenuEntity;
import net.leoch.modules.sys.service.SysRoleMenuService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;


/**
 * 角色与菜单对应关系
 *
 * @author Taohongqiang
 */
@Slf4j
@Service
public class SysRoleMenuServiceImpl extends ServiceImpl<SysRoleMenuMapper, SysRoleMenuEntity> implements SysRoleMenuService {

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
		List<SysRoleMenuEntity> entityList = new ArrayList<>();
		for(Long menuId : menuIdList){
			SysRoleMenuEntity sysRoleMenuEntity = new SysRoleMenuEntity();
			sysRoleMenuEntity.setMenuId(menuId);
			sysRoleMenuEntity.setRoleId(roleId);
			entityList.add(sysRoleMenuEntity);
		}
		this.saveBatch(entityList);
	}

	@Override
	public List<Long> getMenuIdList(Long roleId){
		return this.getBaseMapper().getMenuIdList(roleId);
	}

	@Override
	@Transactional(rollbackFor = Exception.class)
	public void deleteByRoleIds(Long[] roleIds) {
		this.getBaseMapper().deleteByRoleIds(roleIds);
	}

	@Override
	@Transactional(rollbackFor = Exception.class)
	public void deleteByMenuId(Long menuId) {
		this.getBaseMapper().deleteByMenuId(menuId);
	}

}
