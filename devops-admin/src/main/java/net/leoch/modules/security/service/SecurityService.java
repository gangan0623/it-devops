

package net.leoch.modules.security.service;

import net.leoch.modules.security.entity.SysUserTokenEntity;
import net.leoch.modules.security.user.UserDetail;
import net.leoch.modules.sys.entity.SysUserEntity;

import java.util.List;
import java.util.Set;

/**
 * 安全相关接口
 *
 * @author Taohongqiang
 */
public interface SecurityService {
    /**
     * 获取用户权限列表
     */
    Set<String> getUserPermissions(UserDetail user);

    SysUserTokenEntity getByToken(String token);

    /**
     * 根据用户ID，查询用户
     * @param userId
     */
    SysUserEntity getUser(Long userId);

    /**
     * 获取用户对应的部门数据权限
     * @param userId  用户ID
     * @return        返回部门ID列表
     */
    List<Long> getDataScopeList(Long userId);
}
