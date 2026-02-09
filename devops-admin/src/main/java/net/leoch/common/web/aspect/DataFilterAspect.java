

package net.leoch.common.web.aspect;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.qiniu.util.StringUtils;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.core.annotation.DataFilter;
import net.leoch.common.core.base.Constant;
import net.leoch.common.exception.ErrorCode;
import net.leoch.common.exception.ServiceException;
import net.leoch.common.web.interceptor.DataScope;
import net.leoch.common.security.user.SecurityUser;
import net.leoch.common.security.user.UserDetail;
import net.leoch.common.core.enums.SuperAdminEnum;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;
import java.util.List;
import java.util.Map;

/**
 * 数据过滤，切面处理类
 *
 * @author Taohongqiang
 */
@Slf4j
@Aspect
@Component
public class DataFilterAspect {

    @Pointcut("@annotation(net.leoch.common.annotation.DataFilter)")
    public void dataFilterCut() {

    }

    @Before("dataFilterCut()")
    public void dataFilter(JoinPoint point) {
        Object params = point.getArgs()[0];
        if (params != null && params instanceof Map) {
            UserDetail user = SecurityUser.getUser();

            //如果是超级管理员，则不进行数据过滤
            if (user.getSuperAdmin() == SuperAdminEnum.YES.value()) {
                return;
            }

            try {
                //否则进行数据过滤
                Map map = (Map) params;
                String sqlFilter = getSqlFilter(user, point);
                map.put(Constant.SQL_FILTER, new DataScope(sqlFilter));
            } catch (Exception e) {
                log.error("[数据过滤] 获取数据过滤SQL失败，拒绝访问", e);
                throw new ServiceException(ErrorCode.DATA_SCOPE_PARAMS_ERROR);
            }

            return;
        }

        throw new ServiceException(ErrorCode.DATA_SCOPE_PARAMS_ERROR);
    }

    /**
     * 获取数据过滤的SQL
     */
    private String getSqlFilter(UserDetail user, JoinPoint point) throws Exception {
        MethodSignature signature = (MethodSignature) point.getSignature();
        Method method = point.getTarget().getClass().getDeclaredMethod(signature.getName(), signature.getParameterTypes());
        DataFilter dataFilter = method.getAnnotation(DataFilter.class);

        //获取表的别名
        String tableAlias = dataFilter.tableAlias();
        if (StrUtil.isNotBlank(tableAlias)) {
            tableAlias += ".";
        }

        StringBuilder sqlFilter = new StringBuilder();
        sqlFilter.append(" (");

        //部门ID列表
        List<Long> deptIdList = user.getDeptIdList();
        if (CollUtil.isNotEmpty(deptIdList)) {
            sqlFilter.append(tableAlias).append(dataFilter.deptId());

            sqlFilter.append(" in(").append(StringUtils.join(deptIdList, ",")).append(")");
        }

        //查询本人数据
        if (CollUtil.isNotEmpty(deptIdList)) {
            sqlFilter.append(" or ");
        }
        sqlFilter.append(tableAlias).append(dataFilter.userId()).append("=").append(user.getId());

        sqlFilter.append(")");

        return sqlFilter.toString();
    }
}