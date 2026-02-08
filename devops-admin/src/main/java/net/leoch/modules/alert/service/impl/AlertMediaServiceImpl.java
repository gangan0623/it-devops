package net.leoch.modules.alert.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.ConvertUtils;
import net.leoch.modules.alert.mapper.AlertMediaMapper;
import net.leoch.modules.alert.vo.rsp.AlertMediaRsp;
import net.leoch.modules.alert.vo.req.AlertMediaPageReq;
import lombok.extern.slf4j.Slf4j;
import net.leoch.modules.alert.entity.AlertMediaEntity;
import net.leoch.modules.alert.service.IAlertMediaService;
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
public class AlertMediaServiceImpl extends ServiceImpl<AlertMediaMapper, AlertMediaEntity> implements IAlertMediaService {

    @Override
    public PageData<AlertMediaRsp> page(AlertMediaPageReq request) {
        IPage<AlertMediaEntity> page = this.page(request.buildPage(),
            new LambdaQueryWrapper<AlertMediaEntity>()
                .like(StrUtil.isNotBlank(request.getName()), AlertMediaEntity::getName, request.getName())
                .eq(StrUtil.isNotBlank(request.getStatus()), AlertMediaEntity::getStatus, request.getStatus())
        );
        return new PageData<>(ConvertUtils.sourceToTarget(page.getRecords(), AlertMediaRsp.class), page.getTotal());
    }

    @Override
    public List<AlertMediaRsp> list(AlertMediaPageReq request) {
        List<AlertMediaEntity> entityList = this.list(
            new LambdaQueryWrapper<AlertMediaEntity>()
                .like(request != null && StrUtil.isNotBlank(request.getName()), AlertMediaEntity::getName, request != null ? request.getName() : null)
                .eq(request != null && StrUtil.isNotBlank(request.getStatus()), AlertMediaEntity::getStatus, request != null ? request.getStatus() : null)
        );
        return ConvertUtils.sourceToTarget(entityList, AlertMediaRsp.class);
    }

    @Override
    public AlertMediaRsp get(Long id) {
        return ConvertUtils.sourceToTarget(this.getById(id), AlertMediaRsp.class);
    }

    @Override
    public void save(AlertMediaRsp dto) {
        AlertMediaEntity entity = ConvertUtils.sourceToTarget(dto, AlertMediaEntity.class);
        this.save(entity);
        BeanUtils.copyProperties(entity, dto);
    }

    @Override
    public void update(AlertMediaRsp dto) {
        this.updateById(ConvertUtils.sourceToTarget(dto, AlertMediaEntity.class));
    }

    @Override
    public void delete(Long[] ids) {
        this.removeByIds(Arrays.asList(ids));
    }
}
