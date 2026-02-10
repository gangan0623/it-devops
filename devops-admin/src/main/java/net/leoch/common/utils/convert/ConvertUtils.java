

package net.leoch.common.utils.convert;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * 转换工具类
 *
 * @author Taohongqiang
 */
@Slf4j
public class ConvertUtils {

    /**
     * 对象转换
     *
     * @param source 源对象
     * @param target 目标类型
     * @param <T> 泛型类型
     * @return 转换后的对象
     */
    public static <T> T sourceToTarget(Object source, Class<T> target){
        if(source == null){
            return null;
        }
        T targetObject = null;
        try {
            targetObject = target.newInstance();
            BeanUtils.copyProperties(source, targetObject);
        } catch (Exception e) {
            log.error("[对象转换] 转换失败", e);
        }

        return targetObject;
    }

    /**
     * 集合对象转换
     *
     * @param sourceList 源集合
     * @param target 目标类型
     * @param <T> 泛型类型
     * @return 转换后的集合
     */
    public static <T> List<T> sourceToTarget(Collection<?> sourceList, Class<T> target){
        if(sourceList == null){
            return null;
        }

        List targetList = new ArrayList<>(sourceList.size());
        try {
            for(Object source : sourceList){
                T targetObject = target.newInstance();
                BeanUtils.copyProperties(source, targetObject);
                targetList.add(targetObject);
            }
        }catch (Exception e){
            log.error("[对象转换] 转换失败", e);
        }

        return targetList;
    }
}