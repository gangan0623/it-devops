

package net.leoch.modules.security.service.impl;

import cn.hutool.core.util.StrUtil;
import lombok.AllArgsConstructor;
import net.leoch.modules.security.dao.SysUserTokenDao;
import net.leoch.modules.security.entity.SysUserTokenEntity;
import net.leoch.modules.security.service.ShiroService;
import net.leoch.modules.security.user.UserDetail;
import net.leoch.modules.sys.dao.SysMenuDao;
import net.leoch.modules.sys.dao.SysRoleDataScopeDao;
import net.leoch.modules.sys.dao.SysUserDao;
import net.leoch.modules.sys.entity.SysUserEntity;
import net.leoch.modules.sys.enums.SuperAdminEnum;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Service
@AllArgsConstructor
public class ShiroServiceImpl implements ShiroService {
    private final SysMenuDao sysMenuDao;
    private final SysUserDao sysUserDao;
    private final SysUserTokenDao sysUserTokenDao;
    private final SysRoleDataScopeDao sysRoleDataScopeDao;

    @Override
    public Set<String> getUserPermissions(UserDetail user) {
        //系统管理员，拥有最高权限
        List<String> permissionsList;
        if (user.getSuperAdmin() == SuperAdminEnum.YES.value()) {
            permissionsList = sysMenuDao.getPermissionsList();
        } else {
            permissionsList = sysMenuDao.getUserPermissionsList(user.getId());
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
        return sysUserTokenDao.getByToken(token);
    }

    @Override
    public SysUserEntity getUser(Long userId) {
        return sysUserDao.selectById(userId);
    }

    @Override
    public List<Long> getDataScopeList(Long userId) {
        return sysRoleDataScopeDao.getDataScopeList(userId);
    }
}
