package net.leoch.common.config.security;

import cn.dev33.satoken.context.SaHolder;
import cn.dev33.satoken.filter.SaServletFilter;
import cn.dev33.satoken.router.SaRouter;
import cn.dev33.satoken.stp.StpUtil;
import net.leoch.common.exception.ErrorCode;
import net.leoch.common.utils.JsonUtils;
import net.leoch.common.utils.Result;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class SaTokenConfig {

    @Bean
    public SaServletFilter saServletFilter() {
        return new SaServletFilter()
            .addInclude("/**")
            .addExclude(
                "/webjars/**",
                "/login",
                "/swagger/**",
                "/v3/api-docs/**",
                "/doc.html",
                "/swagger-resources/**",
                "/captcha",
                "/favicon.ico",
                "/",
                "/webhook/**",
                "/ops/backup/**",
                "/sd/linux/**",
                "/sd/windows/**",
                "/sd/probe/**"
            )
            .setAuth(obj -> {
                String method = SaHolder.getRequest().getMethod();
                if ("OPTIONS".equalsIgnoreCase(method)) {
                    return;
                }
                SaRouter.match("/**", r -> StpUtil.checkLogin());
            })
            .setError(e -> JsonUtils.toJsonString(new Result().error(ErrorCode.UNAUTHORIZED)));
    }
}
