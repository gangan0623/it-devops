

package net.leoch.common.utils.security;

import net.leoch.common.integration.security.BCryptPasswordEncoder;
import net.leoch.common.integration.security.PasswordEncoder;

/**
 * 密码工具类
 *
 * @author Taohongqiang
 * @since 1.0.0
 */
public class PasswordUtils {

    private static final PasswordEncoder passwordEncoder = new BCryptPasswordEncoder();

    /**
     * 加密
     *
     * @param str 字符串
     * @return 返回加密字符串
     */
    public static String encode(String str) {
        return passwordEncoder.encode(str);
    }


    /**
     * 比较密码是否相等
     *
     * @param str      明文密码
     * @param password 加密后密码
     * @return true：成功    false：失败
     */
    public static boolean matches(String str, String password) {
        return !passwordEncoder.matches(str, password);
    }


}
