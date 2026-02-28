

package net.leoch.common.integration.security;

import cn.dev33.satoken.exception.NotWebContextException;
import cn.dev33.satoken.session.SaSession;
import cn.dev33.satoken.stp.StpUtil;
import cn.hutool.core.bean.BeanUtil;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.utils.context.SpringContextUtils;
import net.leoch.modules.security.service.ISecurityService;
import net.leoch.modules.sys.entity.SysUserEntity;

/**
 * 用户
 *
 * @author Taohongqiang
 */
@Slf4j
public class SecurityUser {
    private static final long SYSTEM_USER_ID = 0L;

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

            ISecurityService securityService = SpringContextUtils.getBean(ISecurityService.class);
            SysUserEntity userEntity = securityService.getUser(StpUtil.getLoginIdAsLong());
            if (userEntity == null) {
                return new UserDetail();
            }

            user = BeanUtil.copyProperties(userEntity, UserDetail.class);
            StpUtil.getSession().set("user", user);
            return user;
        } catch (NotWebContextException e) {
            // 定时任务/异步线程没有 HTTP 上下文，回退系统用户
            return systemUser();
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
        } catch (NotWebContextException e) {
            // 定时任务/异步线程没有 HTTP 上下文，回退系统用户
            return SYSTEM_USER_ID;
        } catch (Exception e) {
            log.warn("[安全] 获取用户ID失败", e);
            return SYSTEM_USER_ID;
        }
    }

    private static UserDetail systemUser() {
        UserDetail user = new UserDetail();
        user.setId(SYSTEM_USER_ID);
        user.setUsername("system");
        user.setRealName("系统任务");
        return user;
    }
}
