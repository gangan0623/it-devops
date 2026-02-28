package net.leoch.modules.sys.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.data.page.PageData;
import net.leoch.modules.sys.entity.SysUserEntity;
import net.leoch.modules.sys.vo.req.SysUserPageReq;
import net.leoch.modules.sys.vo.req.SysUserReq;
import net.leoch.modules.sys.vo.req.UserProfileUpdateReq;
import net.leoch.modules.sys.vo.rsp.SysUserRsp;

import java.util.List;


/**
 * 系统用户
 *
 * @author Taohongqiang
 */
public interface ISysUserService extends IService<SysUserEntity> {

	PageData<SysUserRsp> page(SysUserPageReq request);

	List<SysUserRsp> list(SysUserPageReq request);

	SysUserRsp get(Long id);

	/**
	 * 获取用户详情（含角色列表）
	 * @param id 用户ID
	 * @return 用户详情
	 */
	SysUserRsp getWithRoles(Long id);

	SysUserRsp getByUsername(String username);

	void save(SysUserReq dto);

	void update(SysUserReq dto);

	void delete(Long[] ids);

	/**
	 * 修改密码
	 * @param id           用户ID
	 * @param newPassword  新密码
	 */
	void updatePassword(Long id, String newPassword);

	/**
	 * 修改当前用户密码
	 * @param dto 密码修改请求
	 */
	void changePassword(net.leoch.modules.sys.vo.req.PasswordReq dto);

	/**
	 * 获取当前登录用户信息
	 * @return 当前用户信息
	 */
	SysUserRsp getCurrentUserInfo();

	/**
	 * 修改当前用户个人信息（含密码）
	 * @param dto 个人信息修改请求
	 */
	void updateCurrentUserProfile(UserProfileUpdateReq dto);

	/**
	 * 强制用户下线
	 * @param ids 用户ID数组
	 */
	void kickout(Long[] ids);

}
