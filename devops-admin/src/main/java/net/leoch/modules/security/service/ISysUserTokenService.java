package net.leoch.modules.security.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.utils.Result;
import net.leoch.modules.security.entity.SysUserTokenEntity;

/**
 * 用户Token
 *
 * @author Taohongqiang
 */
public interface ISysUserTokenService extends IService<SysUserTokenEntity> {

	/**
	 * 生成token
	 * @param userId  用户ID
	 */
	Result createToken(Long userId);

	/**
	 * 退出，修改token值
	 * @param userId  用户ID
	 */
	void logout(Long userId);

}
