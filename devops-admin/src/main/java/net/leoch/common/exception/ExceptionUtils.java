
package net.leoch.common.exception;

import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;

/**
 * Exception工具类
 *
 * @author Taohongqiang
 */
@Slf4j
public class ExceptionUtils {

    /**
     * 获取异常信息
     * @param ex  异常
     * @return    返回异常信息
     */
    public static String getErrorStackTrace(Exception ex){
        StringWriter sw = null;
        PrintWriter pw = null;
        try {
            sw = new StringWriter();
            pw = new PrintWriter(sw, true);
            ex.printStackTrace(pw);
        }finally {
            try {
                if(pw != null) {
                    pw.close();
                }
            } catch (Exception e) {
                log.warn("[ExceptionUtils] 关闭PrintWriter失败", e);
            }
            try {
                if(sw != null) {
                    sw.close();
                }
            } catch (IOException e) {
                log.warn("[ExceptionUtils] 关闭StringWriter失败", e);
            }
        }

        return sw.toString();
    }
}