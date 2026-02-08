

package net.leoch.common.utils;

import cn.hutool.core.util.StrUtil;

import java.net.InetAddress;
import java.net.URI;

/**
 * 简单在线检测
 *
 * @author Taohongqiang
 */
public class PingUtils {

    private PingUtils() {
    }

    public static boolean isReachable(String instance, int timeoutMs) {
        if (StrUtil.isBlank(instance)) {
            return false;
        }
        String host = parseHost(instance.trim());
        if (StrUtil.isBlank(host)) {
            return false;
        }
        try {
            InetAddress address = InetAddress.getByName(host);
            return address.isReachable(timeoutMs);
        } catch (Exception e) {
            return false;
        }
    }

    private static String parseHost(String instance) {
        String host = instance;
        try {
            if (instance.startsWith("http://") || instance.startsWith("https://")) {
                URI uri = new URI(instance);
                host = uri.getHost();
            }
        } catch (Exception e) {
            host = instance;
        }
        if (StrUtil.isBlank(host)) {
            host = instance;
        }
        if (host.contains("/")) {
            host = host.split("/")[0];
        }
        if (host.contains(":")) {
            host = host.split(":")[0];
        }
        return host;
    }
}
