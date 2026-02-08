package net.leoch.modules.security.service.impl;

import cn.dev33.satoken.stp.StpUtil;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.constant.Constant;
import net.leoch.common.exception.ErrorCode;
import net.leoch.common.utils.ConvertUtils;
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

@Slf4j
@AllArgsConstructor
@Service
public class SysUserTokenServiceImpl extends ServiceImpl<SysUserTokenDao, SysUserTokenEntity> implements SysUserTokenService {
	private final SysUserService sysUserService;
	private final SecurityService securityService;
	/**
	 * 12小时后过期
	 */
	private static final int EXPIRE = 3600 * 12;

	@Override
	public Result<Object> createToken(Long userId) {
		SysUserDTO user = sysUserService.get(userId);
		if (user == null) {
			return new Result().error(ErrorCode.ACCOUNT_PASSWORD_ERROR);
		}

		StpUtil.login(userId);
		String token = StpUtil.getTokenValue();
		long timeout = StpUtil.getTokenTimeout();

		UserDetail userDetail = ConvertUtils.sourceToTarget(user, UserDetail.class);
		userDetail.setDeptIdList(securityService.getDataScopeList(userId));
		StpUtil.getSession().set("user", userDetail);

		Date now = new Date();
		Date expireTime = timeout > 0 ? new Date(now.getTime() + timeout * 1000) : null;

		SysUserTokenEntity tokenEntity = this.getBaseMapper().getByUserId(userId);
		if (tokenEntity == null) {
			tokenEntity = new SysUserTokenEntity();
			tokenEntity.setUserId(userId);
			tokenEntity.setToken(token);
			tokenEntity.setUpdateDate(now);
			tokenEntity.setExpireDate(expireTime);
			this.save(tokenEntity);
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
		this.getBaseMapper().updateToken(userId, token);
	}
}
