

package net.leoch.modules.security.service;

import jakarta.servlet.http.HttpServletRequest;
import net.leoch.common.data.result.Result;
import net.leoch.common.integration.security.UserDetail;
import net.leoch.modules.security.vo.req.LoginReq;
import net.leoch.modules.sys.entity.SysUserEntity;

import java.util.Set;

/**
 * 安全相关接口
 *
 * @author Taohongqiang
 */
public interface ISecurityService {
    /**
     * 获取用户权限列表
     */
    Set<String> getUserPermissions(UserDetail user);

    /**
     * 根据用户ID，查询用户
     * @param userId 用户ID
     */
    SysUserEntity getUser(Long userId);

    /**
     * 用户登录
     * @param request HTTP请求
     * @param login   登录信息
     * @return        登录结果
     */
    Result<Object> login(HttpServletRequest request, LoginReq login);

    /**
     * 记录退出日志并注销
     * @param request HTTP请求
     */
    void recordLogout(HttpServletRequest request);
}
