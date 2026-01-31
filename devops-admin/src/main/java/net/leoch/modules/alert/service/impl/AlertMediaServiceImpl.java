package net.leoch.modules.alert.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import net.leoch.common.service.impl.CrudServiceImpl;
import net.leoch.modules.alert.dao.AlertMediaDao;
import net.leoch.modules.alert.dto.AlertMediaDTO;
import net.leoch.modules.alert.entity.AlertMediaEntity;
import net.leoch.modules.alert.service.AlertMediaService;
import org.springframework.stereotype.Service;

import java.util.Map;

/**
 * 告警媒介
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Service
public class AlertMediaServiceImpl extends CrudServiceImpl<AlertMediaDao, AlertMediaEntity, AlertMediaDTO> implements AlertMediaService {

    @Override
    public QueryWrapper<AlertMediaEntity> getWrapper(Map<String, Object> params) {
        String name = (String) params.get("name");
        String status = (String) params.get("status");

        QueryWrapper<AlertMediaEntity> wrapper = new QueryWrapper<>();
        wrapper.like(StrUtil.isNotBlank(name), "name", name);
        wrapper.eq(StrUtil.isNotBlank(status), "status", status);

        return wrapper;
    }
}
