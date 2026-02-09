

package net.leoch.modules.security.service.impl;

import com.wf.captcha.SpecCaptcha;
import com.wf.captcha.base.Captcha;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.exception.ErrorCode;
import net.leoch.common.integration.redis.RedisKeys;
import net.leoch.common.integration.redis.RedisUtils;
import net.leoch.common.data.validator.AssertUtils;
import net.leoch.modules.security.service.ICaptchaService;
import org.springframework.stereotype.Service;

import java.io.IOException;

/**
 * 验证码
 *
 * @author Taohongqiang
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class CaptchaServiceImpl implements ICaptchaService {
    private final RedisUtils redisUtils;

    @Override
    public void create(HttpServletResponse response, String uuid) throws IOException {
        AssertUtils.isBlank(uuid, ErrorCode.IDENTIFIER_NOT_NULL);

        response.setContentType("image/gif");
        response.setHeader("Pragma", "No-cache");
        response.setHeader("Cache-Control", "no-cache");
        response.setDateHeader("Expires", 0);

        //生成验证码
        SpecCaptcha captcha = new SpecCaptcha(150, 40);
        captcha.setLen(5);
        captcha.setCharType(Captcha.TYPE_DEFAULT);
        captcha.out(response.getOutputStream());

        //保存到缓存
        setCache(uuid, captcha.text());
    }

    @Override
    public boolean validate(String uuid, String code) {
        //获取验证码
        String captcha = getCache(uuid);
        //效验成功
        return code.equalsIgnoreCase(captcha);
    }

    private void setCache(String key, String value) {
        key = RedisKeys.getCaptchaKey(key);
        redisUtils.set(key, value, 300);
    }

    private String getCache(String key) {
        key = RedisKeys.getCaptchaKey(key);
        String captcha = (String) redisUtils.get(key);
        //删除验证码
        if (captcha != null) {
            redisUtils.delete(key);
        }
        return captcha;
    }
}
