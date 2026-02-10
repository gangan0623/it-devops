

package net.leoch.modules.security.service.impl;

import cn.hutool.core.util.StrUtil;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.exception.ErrorCode;
import net.leoch.common.exception.ServiceException;
import net.leoch.common.utils.IpUtils;
import net.leoch.common.data.result.Result;
import net.leoch.common.data.validator.ValidatorUtils;
import net.leoch.modules.log.entity.SysLogLoginEntity;
import net.leoch.common.enums.LoginOperationEnum;
import net.leoch.common.enums.LoginStatusEnum;
import net.leoch.modules.log.service.ISysLogLoginService;
import net.leoch.modules.security.mapper.SysUserTokenMapper;
import net.leoch.modules.security.vo.req.LoginReq;
import net.leoch.modules.security.entity.SysUserTokenEntity;
import net.leoch.common.utils.security.PasswordUtils;
import net.leoch.modules.security.service.ICaptchaService;
import net.leoch.modules.security.service.ISecurityService;
import net.leoch.modules.security.service.ISysUserTokenService;
import net.leoch.common.integration.security.SecurityUser;
import net.leoch.common.integration.security.UserDetail;
import net.leoch.modules.sys.mapper.SysMenuMapper;
import net.leoch.modules.sys.mapper.SysRoleDataScopeMapper;
import net.leoch.modules.sys.mapper.SysUserMapper;
import net.leoch.modules.sys.vo.rsp.SysUserRsp;
import net.leoch.modules.sys.entity.SysUserEntity;
import net.leoch.common.enums.SuperAdminEnum;
import net.leoch.common.enums.UserStatusEnum;
import net.leoch.modules.sys.service.ISysUserService;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Slf4j
@Service
@RequiredArgsConstructor
public class SecurityServiceImpl implements ISecurityService {
    private final SysMenuMapper sysMenuMapper;
    private final SysUserMapper sysUserMapper;
    private final SysUserTokenMapper sysUserTokenMapper;
    private final SysRoleDataScopeMapper sysRoleDataScopeMapper;
    private final ISysUserService sysUserService;
    private final ISysUserTokenService sysUserTokenService;
    private final ICaptchaService captchaService;
    private final ISysLogLoginService sysLogLoginService;

    @Override
    public Set<String> getUserPermissions(UserDetail user) {
        //系统管理员，拥有最高权限
        List<String> permissionsList;
        if (user.getSuperAdmin() == SuperAdminEnum.YES.value()) {
            permissionsList = sysMenuMapper.getPermissionsList();
        } else {
            permissionsList = sysMenuMapper.getUserPermissionsList(user.getId());
        }

        //用户权限列表
        Set<String> permsSet = new HashSet<>();
        if (user.getSuperAdmin() == SuperAdminEnum.YES.value()) {
            permsSet.add("*:*:*");
        }
        for (String permissions : permissionsList) {
            if (StrUtil.isBlank(permissions)) {
                continue;
            }
            permsSet.addAll(Arrays.asList(permissions.trim().split(",")));
        }

        return permsSet;
    }

    @Override
    public SysUserTokenEntity getByToken(String token) {
        return sysUserTokenMapper.getByToken(token);
    }

    @Override
    public SysUserEntity getUser(Long userId) {
        return sysUserMapper.selectById(userId);
    }

    @Override
    public List<Long> getDataScopeList(Long userId) {
        return sysRoleDataScopeMapper.getDataScopeList(userId);
    }

    @Override
    public Result<Object> login(HttpServletRequest request, LoginReq login) {
        ValidatorUtils.validateEntity(login);

        boolean flag = captchaService.validate(login.getUuid(), login.getCaptcha());
        if (!flag) {
            return new Result<>().error(ErrorCode.CAPTCHA_ERROR);
        }

        SysUserRsp user = sysUserService.getByUsername(login.getUsername());

        SysLogLoginEntity loginLog = new SysLogLoginEntity();
        loginLog.setOperation(LoginOperationEnum.LOGIN.value());
        loginLog.setCreateDate(new Date());
        loginLog.setIp(IpUtils.getIpAddr(request));
        loginLog.setUserAgent(request.getHeader(HttpHeaders.USER_AGENT));

        if (user == null) {
            loginLog.setStatus(LoginStatusEnum.FAIL.value());
            loginLog.setCreatorName(login.getUsername());
            sysLogLoginService.save(loginLog);
            throw new ServiceException(ErrorCode.ACCOUNT_PASSWORD_ERROR);
        }

        if (PasswordUtils.matches(login.getPassword(), user.getPassword())) {
            loginLog.setStatus(LoginStatusEnum.FAIL.value());
            loginLog.setCreator(user.getId());
            loginLog.setCreatorName(user.getUsername());
            sysLogLoginService.save(loginLog);
            throw new ServiceException(ErrorCode.ACCOUNT_PASSWORD_ERROR);
        }

        if (user.getStatus() == UserStatusEnum.DISABLE.value()) {
            loginLog.setStatus(LoginStatusEnum.LOCK.value());
            loginLog.setCreator(user.getId());
            loginLog.setCreatorName(user.getUsername());
            sysLogLoginService.save(loginLog);
            throw new ServiceException(ErrorCode.ACCOUNT_DISABLE);
        }

        loginLog.setStatus(LoginStatusEnum.SUCCESS.value());
        loginLog.setCreator(user.getId());
        loginLog.setCreatorName(user.getUsername());
        sysLogLoginService.save(loginLog);

        return sysUserTokenService.createToken(user.getId());
    }

    @Override
    public void recordLogout(HttpServletRequest request) {
        UserDetail user = SecurityUser.getUser();
        sysUserTokenService.logout(user.getId());

        SysLogLoginEntity loginLog = new SysLogLoginEntity();
        loginLog.setOperation(LoginOperationEnum.LOGOUT.value());
        loginLog.setIp(IpUtils.getIpAddr(request));
        loginLog.setUserAgent(request.getHeader(HttpHeaders.USER_AGENT));
        loginLog.setStatus(LoginStatusEnum.SUCCESS.value());
        loginLog.setCreator(user.getId());
        loginLog.setCreatorName(user.getUsername());
        loginLog.setCreateDate(new Date());
        sysLogLoginService.save(loginLog);
    }
}
