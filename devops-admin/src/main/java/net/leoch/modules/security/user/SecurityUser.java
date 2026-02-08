

package net.leoch.modules.security.user;

import cn.dev33.satoken.session.SaSession;
import cn.dev33.satoken.stp.StpUtil;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.utils.ConvertUtils;
import net.leoch.common.utils.SpringContextUtils;
import net.leoch.modules.security.service.SecurityService;
import net.leoch.modules.sys.entity.SysUserEntity;

/**
 * 用户
 *
 * @author Taohongqiang
 */
@Slf4j
public class SecurityUser {

    /**
     * 获取用户信息
     */
    public static UserDetail getUser() {
        try {
            if (!StpUtil.isLogin()) {
                return new UserDetail();
            }

            SaSession session = StpUtil.getSession(false);
            if (session == null) {
                return new UserDetail();
            }

            UserDetail user = session.getModel("user", UserDetail.class);
            if (user != null) {
                return user;
            }

            SecurityService securityService = SpringContextUtils.getBean(SecurityService.class);
            SysUserEntity userEntity = securityService.getUser(StpUtil.getLoginIdAsLong());
            if (userEntity == null) {
                return new UserDetail();
            }

            user = ConvertUtils.sourceToTarget(userEntity, UserDetail.class);
            user.setDeptIdList(securityService.getDataScopeList(user.getId()));
            StpUtil.getSession().set("user", user);
            return user;
        } catch (Exception e) {
            log.warn("[安全] 获取用户信息失败", e);
            return new UserDetail();
        }
    }

    /**
     * 获取用户ID
     */
    public static Long getUserId() {
        try {
            return StpUtil.getLoginIdAsLong();
        } catch (Exception e) {
            log.warn("[安全] 获取用户ID失败", e);
            return null;
        }
    }

    /**
     * 获取部门ID
     */
    public static Long getDeptId() {
        return getUser().getDeptId();
    }
}
