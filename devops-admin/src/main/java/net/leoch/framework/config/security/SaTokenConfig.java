package net.leoch.framework.config.security;

import cn.dev33.satoken.context.SaHolder;
import cn.dev33.satoken.filter.SaServletFilter;
import cn.dev33.satoken.router.SaRouter;
import cn.dev33.satoken.stp.StpUtil;
import cn.hutool.json.JSONUtil;
import net.leoch.common.data.result.Result;
import net.leoch.common.exception.ErrorCode;
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
                "/swagger-ui.html",
                "/swagger-ui/**",
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
            .setError(e -> JSONUtil.toJsonStr(new Result().error(ErrorCode.UNAUTHORIZED)));
    }
}
