


package net.leoch.modules.security.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.AllArgsConstructor;
import net.leoch.common.exception.ErrorCode;
import net.leoch.common.utils.Result;
import net.leoch.common.validator.AssertUtils;
import net.leoch.modules.security.vo.req.LoginReq;
import net.leoch.modules.security.service.ICaptchaService;
import net.leoch.modules.security.service.ISecurityService;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import java.io.IOException;

/**
 * 登录
 *
 * @author Taohongqiang
 */
@RestController
@Tag(name = "登录管理")
@AllArgsConstructor
public class LoginController {
    private final ISecurityService securityService;
    private final ICaptchaService captchaService;

    @GetMapping("captcha")
    @Operation(summary = "验证码")
    @Parameter(in = ParameterIn.QUERY, ref = "string", name = "uuid", required = true)
    public void captcha(HttpServletResponse response, String uuid) throws IOException {
        //uuid不能为空
        AssertUtils.isBlank(uuid, ErrorCode.IDENTIFIER_NOT_NULL);

        //生成验证码
        captchaService.create(response, uuid);
    }

    @PostMapping("login")
    @Operation(summary = "登录")
    public Result<Object> login(HttpServletRequest request, @RequestBody LoginReq login) {
        return securityService.login(request, login);
    }

    @PostMapping("logout")
    @Operation(summary = "退出")
    public Result<Object> logout(HttpServletRequest request) {
        securityService.recordLogout(request);
        return new Result<>();
    }

}
