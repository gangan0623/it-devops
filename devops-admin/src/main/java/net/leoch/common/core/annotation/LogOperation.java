

package net.leoch.common.core.annotation;

import java.lang.annotation.*;

/**
 * 操作日志注解
 *
 * @author Taohongqiang
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface LogOperation {

	String value() default "";
}
