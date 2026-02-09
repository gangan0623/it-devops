package net.leoch.modules.sys.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.modules.security.user.UserDetail;
import net.leoch.modules.sys.vo.req.SysMenuReq;
import net.leoch.modules.sys.vo.rsp.SysMenuRsp;
import net.leoch.modules.sys.entity.SysMenuEntity;

import java.util.List;


/**
 * 菜单管理
 *
 * @author Taohongqiang
 */
public interface ISysMenuService extends IService<SysMenuEntity> {

	SysMenuRsp get(Long id);

	void save(SysMenuReq dto);

	void update(SysMenuReq dto);

	void delete(Long id);

	/**
	 * 菜单列表
	 *
	 * @param menuType 菜单类型
	 */
	List<SysMenuRsp> getAllMenuList(Integer menuType);

	/**
	 * 用户菜单列表
	 *
	 * @param user  用户
	 * @param menuType 菜单类型
	 */
	List<SysMenuRsp> getUserMenuList(UserDetail user, Integer menuType);

	/**
	 * 根据父菜单，查询子菜单
	 * @param pid  父菜单ID
	 */
	List<SysMenuRsp> getListPid(Long pid);
}
