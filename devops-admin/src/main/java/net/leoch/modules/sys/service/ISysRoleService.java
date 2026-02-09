package net.leoch.modules.sys.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.data.page.PageData;
import net.leoch.modules.sys.vo.req.SysRolePageReq;
import net.leoch.modules.sys.vo.req.SysRoleReq;
import net.leoch.modules.sys.vo.rsp.SysRoleRsp;
import net.leoch.modules.sys.entity.SysRoleEntity;

import java.util.List;


/**
 * 角色
 *
 * @author Taohongqiang
 */
public interface ISysRoleService extends IService<SysRoleEntity> {

	PageData<SysRoleRsp> page(SysRolePageReq request);

	List<SysRoleRsp> list(SysRolePageReq request);

	SysRoleRsp get(Long id);

	/**
	 * 获取角色详情（含菜单和数据权限）
	 * @param id 角色ID
	 * @return 角色详情
	 */
	SysRoleRsp getWithMenuAndDataScope(Long id);

	void save(SysRoleReq dto);

	void update(SysRoleReq dto);

	void delete(Long[] ids);

}
