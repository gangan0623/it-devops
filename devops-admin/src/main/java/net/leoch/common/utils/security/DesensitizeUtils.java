package net.leoch.common.utils.security;

/**
 * 敏感信息脱敏工具类
 *
 * @author Claude
 */
public class DesensitizeUtils {

    /**
     * 手机号脱敏：保留前3位和后4位
     * 例如：13812345678 -> 138****5678
     *
     * @param mobile 手机号
     * @return 脱敏后的手机号
     */
    public static String mobile(String mobile) {
        if (mobile == null || mobile.length() != 11) {
            return mobile;
        }
        return mobile.substring(0, 3) + "****" + mobile.substring(7);
    }

    /**
     * 身份证号脱敏：保留前6位和后4位
     * 例如：110101199001011234 -> 110101********1234
     *
     * @param idCard 身份证号
     * @return 脱敏后的身份证号
     */
    public static String idCard(String idCard) {
        if (idCard == null || idCard.length() < 10) {
            return idCard;
        }
        int length = idCard.length();
        return idCard.substring(0, 6) + "********" + idCard.substring(length - 4);
    }

    /**
     * 密码脱敏：完全隐藏
     *
     * @param password 密码
     * @return 固定返回 "******"
     */
    public static String password(String password) {
        return password == null || password.isEmpty() ? password : "******";
    }

    /**
     * Token脱敏：仅保留前8位
     * 例如：abcdef1234567890 -> abcdef12********
     *
     * @param token token字符串
     * @return 脱敏后的token
     */
    public static String token(String token) {
        if (token == null || token.length() <= 8) {
            return "********";
        }
        return token.substring(0, 8) + "********";
    }

    /**
     * 邮箱脱敏：保留@前1-2位和@后的域名
     * 例如：example@gmail.com -> ex****@gmail.com
     *
     * @param email 邮箱地址
     * @return 脱敏后的邮箱
     */
    public static String email(String email) {
        if (email == null || !email.contains("@")) {
            return email;
        }
        int atIndex = email.indexOf("@");
        String prefix = email.substring(0, atIndex);
        String domain = email.substring(atIndex);

        if (prefix.length() <= 2) {
            return prefix + "****" + domain;
        }
        return prefix.substring(0, 2) + "****" + domain;
    }

    /**
     * IP地址脱敏：保留前两段
     * 例如：192.168.1.100 -> 192.168.*.*
     *
     * @param ip IP地址
     * @return 脱敏后的IP
     */
    public static String ip(String ip) {
        if (ip == null || !ip.contains(".")) {
            return ip;
        }
        String[] parts = ip.split("\\.");
        if (parts.length != 4) {
            return ip;
        }
        return parts[0] + "." + parts[1] + ".*.*";
    }

    /**
     * 银行卡号脱敏：保留前4位和后4位
     * 例如：6222021234567890123 -> 6222 **** **** 0123
     *
     * @param bankCard 银行卡号
     * @return 脱敏后的银行卡号
     */
    public static String bankCard(String bankCard) {
        if (bankCard == null || bankCard.length() < 8) {
            return bankCard;
        }
        int length = bankCard.length();
        return bankCard.substring(0, 4) + " **** **** " + bankCard.substring(length - 4);
    }

    /**
     * 通用脱敏：保留前后各N位
     *
     * @param str         原始字符串
     * @param prefixLen   保留前缀长度
     * @param suffixLen   保留后缀长度
     * @param maskChar    脱敏字符（默认*）
     * @return 脱敏后的字符串
     */
    public static String custom(String str, int prefixLen, int suffixLen, char maskChar) {
        if (str == null || str.length() <= prefixLen + suffixLen) {
            return str;
        }
        int maskLen = str.length() - prefixLen - suffixLen;
        StringBuilder mask = new StringBuilder();
        for (int i = 0; i < maskLen; i++) {
            mask.append(maskChar);
        }
        return str.substring(0, prefixLen) + mask + str.substring(str.length() - suffixLen);
    }
}
