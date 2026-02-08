package net.leoch.modules.alert.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.ConvertUtils;
import net.leoch.modules.alert.mapper.AlertMediaMapper;
import net.leoch.modules.alert.dto.AlertMediaDTO;
import net.leoch.modules.alert.dto.AlertMediaPageRequest;
import lombok.extern.slf4j.Slf4j;
import net.leoch.modules.alert.entity.AlertMediaEntity;
import net.leoch.modules.alert.service.AlertMediaService;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.List;

/**
 * 告警媒介
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Slf4j
@Service
public class AlertMediaServiceImpl extends ServiceImpl<AlertMediaMapper, AlertMediaEntity> implements AlertMediaService {

    @Override
    public PageData<AlertMediaDTO> page(AlertMediaPageRequest request) {
        IPage<AlertMediaEntity> page = this.page(request.buildPage(),
            new LambdaQueryWrapper<AlertMediaEntity>()
                .like(StrUtil.isNotBlank(request.getName()), AlertMediaEntity::getName, request.getName())
                .eq(StrUtil.isNotBlank(request.getStatus()), AlertMediaEntity::getStatus, request.getStatus())
        );
        return new PageData<>(ConvertUtils.sourceToTarget(page.getRecords(), AlertMediaDTO.class), page.getTotal());
    }

    @Override
    public List<AlertMediaDTO> list(AlertMediaPageRequest request) {
        List<AlertMediaEntity> entityList = this.list(
            new LambdaQueryWrapper<AlertMediaEntity>()
                .like(request != null && StrUtil.isNotBlank(request.getName()), AlertMediaEntity::getName, request != null ? request.getName() : null)
                .eq(request != null && StrUtil.isNotBlank(request.getStatus()), AlertMediaEntity::getStatus, request != null ? request.getStatus() : null)
        );
        return ConvertUtils.sourceToTarget(entityList, AlertMediaDTO.class);
    }

    @Override
    public AlertMediaDTO get(Long id) {
        return ConvertUtils.sourceToTarget(this.getById(id), AlertMediaDTO.class);
    }

    @Override
    public void save(AlertMediaDTO dto) {
        AlertMediaEntity entity = ConvertUtils.sourceToTarget(dto, AlertMediaEntity.class);
        this.save(entity);
        BeanUtils.copyProperties(entity, dto);
    }

    @Override
    public void update(AlertMediaDTO dto) {
        this.updateById(ConvertUtils.sourceToTarget(dto, AlertMediaEntity.class));
    }

    @Override
    public void delete(Long[] ids) {
        this.removeByIds(Arrays.asList(ids));
    }
}
