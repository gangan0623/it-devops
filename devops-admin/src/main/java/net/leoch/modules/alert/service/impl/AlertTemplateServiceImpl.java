package net.leoch.modules.alert.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.ConvertUtils;
import net.leoch.modules.alert.mapper.AlertTemplateMapper;
import net.leoch.modules.alert.dto.AlertTemplateDTO;
import net.leoch.modules.alert.dto.AlertTemplatePageRequest;
import lombok.extern.slf4j.Slf4j;
import net.leoch.modules.alert.entity.AlertTemplateEntity;
import net.leoch.modules.alert.service.AlertTemplateService;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.List;

/**
 * 告警模板
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Slf4j
@Service
public class AlertTemplateServiceImpl extends ServiceImpl<AlertTemplateMapper, AlertTemplateEntity> implements AlertTemplateService {

    @Override
    public PageData<AlertTemplateDTO> page(AlertTemplatePageRequest request) {
        IPage<AlertTemplateEntity> page = this.page(request.buildPage(),
            new LambdaQueryWrapper<AlertTemplateEntity>()
                .like(StrUtil.isNotBlank(request.getName()), AlertTemplateEntity::getName, request.getName())
                .eq(StrUtil.isNotBlank(request.getStatus()), AlertTemplateEntity::getStatus, request.getStatus())
        );
        return new PageData<>(ConvertUtils.sourceToTarget(page.getRecords(), AlertTemplateDTO.class), page.getTotal());
    }

    @Override
    public List<AlertTemplateDTO> list(AlertTemplatePageRequest request) {
        List<AlertTemplateEntity> entityList = this.list(
            new LambdaQueryWrapper<AlertTemplateEntity>()
                .like(request != null && StrUtil.isNotBlank(request.getName()), AlertTemplateEntity::getName, request != null ? request.getName() : null)
                .eq(request != null && StrUtil.isNotBlank(request.getStatus()), AlertTemplateEntity::getStatus, request != null ? request.getStatus() : null)
        );
        return ConvertUtils.sourceToTarget(entityList, AlertTemplateDTO.class);
    }

    @Override
    public AlertTemplateDTO get(Long id) {
        return ConvertUtils.sourceToTarget(this.getById(id), AlertTemplateDTO.class);
    }

    @Override
    public void save(AlertTemplateDTO dto) {
        AlertTemplateEntity entity = ConvertUtils.sourceToTarget(dto, AlertTemplateEntity.class);
        this.save(entity);
        BeanUtils.copyProperties(entity, dto);
    }

    @Override
    public void update(AlertTemplateDTO dto) {
        this.updateById(ConvertUtils.sourceToTarget(dto, AlertTemplateEntity.class));
    }

    @Override
    public void delete(Long[] ids) {
        this.removeByIds(Arrays.asList(ids));
    }
}
