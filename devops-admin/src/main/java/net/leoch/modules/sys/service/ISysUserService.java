package net.leoch.modules.sys.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.page.PageData;
import net.leoch.modules.sys.dto.SysUserDTO;
import net.leoch.modules.sys.dto.SysUserPageRequest;
import net.leoch.modules.sys.entity.SysUserEntity;

import java.util.List;


/**
 * 系统用户
 *
 * @author Taohongqiang
 */
public interface ISysUserService extends IService<SysUserEntity> {

	PageData<SysUserDTO> page(SysUserPageRequest request);

	List<SysUserDTO> list(SysUserPageRequest request);

	SysUserDTO get(Long id);

	SysUserDTO getByUsername(String username);

	void save(SysUserDTO dto);

	void update(SysUserDTO dto);

	void delete(Long[] ids);

	/**
	 * 修改密码
	 * @param id           用户ID
	 * @param newPassword  新密码
	 */
	void updatePassword(Long id, String newPassword);

	/**
	 * 根据部门ID，查询用户数
	 */
	int getCountByDeptId(Long deptId);

	/**
	 * 根据部门ID,查询用户Id列表
	 */
	List<Long> getUserIdListByDeptId(List<Long> deptIdList);

}
