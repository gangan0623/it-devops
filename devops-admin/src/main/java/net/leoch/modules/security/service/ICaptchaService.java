

package net.leoch.modules.security.service;

import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;

/**
 * 验证码
 *
 * @author Taohongqiang
 */
public interface ICaptchaService {

    /**
     * 图片验证码
     */
    void create(HttpServletResponse response, String uuid) throws IOException;

    /**
     * 验证码效验
     * @param uuid  uuid
     * @param code  验证码
     * @return  true：成功  false：失败
     */
    boolean validate(String uuid, String code);
}
