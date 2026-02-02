

package net.leoch.modules.security.service.impl;

import cn.dev33.satoken.stp.StpUtil;
import lombok.AllArgsConstructor;
import net.leoch.common.constant.Constant;
import net.leoch.common.exception.ErrorCode;
import net.leoch.common.service.impl.BaseServiceImpl;
import net.leoch.common.utils.Result;
import net.leoch.modules.security.dao.SysUserTokenDao;
import net.leoch.modules.security.entity.SysUserTokenEntity;
import net.leoch.modules.security.service.SecurityService;
import net.leoch.modules.security.service.SysUserTokenService;
import net.leoch.modules.security.user.UserDetail;
import net.leoch.modules.sys.dto.SysUserDTO;
import net.leoch.modules.sys.service.SysUserService;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

@AllArgsConstructor
@Service
public class SysUserTokenServiceImpl extends BaseServiceImpl<SysUserTokenDao, SysUserTokenEntity> implements SysUserTokenService {
	private final SysUserService sysUserService;
	private final SecurityService securityService;
	/**
	 * 12小时后过期
	 */
	private final static int EXPIRE = 3600 * 12;

	@Override
	public Result<Object> createToken(Long userId) {
		SysUserDTO user = sysUserService.get(userId);
		if (user == null) {
			return new Result().error(ErrorCode.ACCOUNT_PASSWORD_ERROR);
		}

		StpUtil.login(userId);
		String token = StpUtil.getTokenValue();
		long timeout = StpUtil.getTokenTimeout();

		UserDetail userDetail = net.leoch.common.utils.ConvertUtils.sourceToTarget(user, UserDetail.class);
		userDetail.setDeptIdList(securityService.getDataScopeList(userId));
		StpUtil.getSession().set("user", userDetail);

		Date now = new Date();
		Date expireTime = timeout > 0 ? new Date(now.getTime() + timeout * 1000) : null;

		SysUserTokenEntity tokenEntity = baseDao.getByUserId(userId);
		if (tokenEntity == null) {
			tokenEntity = new SysUserTokenEntity();
			tokenEntity.setUserId(userId);
			tokenEntity.setToken(token);
			tokenEntity.setUpdateDate(now);
			tokenEntity.setExpireDate(expireTime);
			this.insert(tokenEntity);
		} else {
			tokenEntity.setToken(token);
			tokenEntity.setUpdateDate(now);
			tokenEntity.setExpireDate(expireTime);
			this.updateById(tokenEntity);
		}

		Map<String, Object> map = new HashMap<>(2);
		map.put(Constant.TOKEN_HEADER, token);
		map.put("expire", timeout > 0 ? timeout : EXPIRE);
		return new Result().ok(map);
	}

	@Override
	public void logout(Long userId) {
		StpUtil.logout(userId);
		String token = UUID.randomUUID().toString().replace("-", "");
		baseDao.updateToken(userId, token);
	}
}
