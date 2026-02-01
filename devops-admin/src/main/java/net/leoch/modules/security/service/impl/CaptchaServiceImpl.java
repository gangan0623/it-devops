

package net.leoch.modules.security.service.impl;

import com.wf.captcha.SpecCaptcha;
import com.wf.captcha.base.Captcha;
import jakarta.annotation.Resource;
import jakarta.servlet.http.HttpServletResponse;
import net.leoch.common.redis.RedisKeys;
import net.leoch.common.redis.RedisUtils;
import net.leoch.modules.security.service.CaptchaService;
import org.springframework.stereotype.Service;

import java.io.IOException;

/**
 * 验证码
 *
 * @author Taohongqiang
 */
@Service
public class CaptchaServiceImpl implements CaptchaService {
    @Resource
    private RedisUtils redisUtils;

    @Override
    public void create(HttpServletResponse response, String uuid) throws IOException {
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
