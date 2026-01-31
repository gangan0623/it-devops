package net.leoch.modules.alert.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import net.leoch.common.service.impl.CrudServiceImpl;
import net.leoch.modules.alert.dao.AlertTemplateDao;
import net.leoch.modules.alert.dto.AlertTemplateDTO;
import net.leoch.modules.alert.entity.AlertTemplateEntity;
import net.leoch.modules.alert.service.AlertTemplateService;
import org.springframework.stereotype.Service;

import java.util.Map;

/**
 * 告警模板
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Service
public class AlertTemplateServiceImpl extends CrudServiceImpl<AlertTemplateDao, AlertTemplateEntity, AlertTemplateDTO> implements AlertTemplateService {

    @Override
    public QueryWrapper<AlertTemplateEntity> getWrapper(Map<String, Object> params) {
        String name = (String) params.get("name");
        String status = (String) params.get("status");

        QueryWrapper<AlertTemplateEntity> wrapper = new QueryWrapper<>();
        wrapper.like(StrUtil.isNotBlank(name), "name", name);
        wrapper.eq(StrUtil.isNotBlank(status), "status", status);

        return wrapper;
    }
}
