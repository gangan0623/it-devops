package net.leoch.modules.security.config;

import cn.dev33.satoken.stp.StpInterface;
import lombok.RequiredArgsConstructor;
import net.leoch.modules.security.service.ISecurityService;
import net.leoch.modules.security.user.UserDetail;
import net.leoch.modules.sys.entity.SysUserEntity;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

@Component
@RequiredArgsConstructor
public class SaTokenStpInterface implements StpInterface {
    private final ISecurityService securityService;

    @Override
    public List<String> getPermissionList(Object loginId, String loginType) {
        if (loginId == null) {
            return Collections.emptyList();
        }
        Long userId = Long.parseLong(String.valueOf(loginId));
        SysUserEntity userEntity = securityService.getUser(userId);
        if (userEntity == null) {
            return Collections.emptyList();
        }
        UserDetail user = new UserDetail();
        user.setId(userEntity.getId());
        user.setSuperAdmin(userEntity.getSuperAdmin());
        Set<String> permissions = securityService.getUserPermissions(user);
        return new ArrayList<>(permissions);
    }

    @Override
    public List<String> getRoleList(Object loginId, String loginType) {
        return Collections.emptyList();
    }
}
