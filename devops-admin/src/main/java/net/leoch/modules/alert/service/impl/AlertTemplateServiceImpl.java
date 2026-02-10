package net.leoch.modules.alert.service.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import net.leoch.common.exception.ServiceException;
import net.leoch.common.data.page.PageData;
import cn.hutool.core.bean.BeanUtil;
import net.leoch.common.data.validator.AssertUtils;
import net.leoch.modules.alert.mapper.AlertTemplateMapper;
import net.leoch.modules.alert.vo.req.AlertTemplatePageReq;
import net.leoch.modules.alert.vo.req.AlertTemplateReq;
import net.leoch.modules.alert.vo.rsp.AlertTemplateRsp;
import net.leoch.modules.alert.vo.req.AlertTemplatePreviewReq;
import lombok.extern.slf4j.Slf4j;
import net.leoch.modules.alert.entity.AlertTemplateEntity;
import net.leoch.modules.alert.service.IAlertTemplateService;
import net.leoch.common.utils.alert.AlertJsonUtils;
import net.leoch.common.utils.alert.AlertPayloadUtils;
import net.leoch.common.utils.alert.AlertTemplateRenderer;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 告警模板
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Slf4j
@Service
public class AlertTemplateServiceImpl extends ServiceImpl<AlertTemplateMapper, AlertTemplateEntity> implements IAlertTemplateService {

    @Override
    public PageData<AlertTemplateRsp> page(AlertTemplatePageReq request) {
        IPage<AlertTemplateEntity> page = this.page(request.buildPage(),
            new LambdaQueryWrapper<AlertTemplateEntity>()
                .like(StrUtil.isNotBlank(request.getName()), AlertTemplateEntity::getName, request.getName())
                .eq(StrUtil.isNotBlank(request.getStatus()), AlertTemplateEntity::getStatus, request.getStatus())
        );
        return new PageData<>(BeanUtil.copyToList(page.getRecords(), AlertTemplateRsp.class), page.getTotal());
    }

    @Override
    public List<AlertTemplateRsp> list(AlertTemplatePageReq request) {
        List<AlertTemplateEntity> entityList = this.list(
            new LambdaQueryWrapper<AlertTemplateEntity>()
                .like(request != null && StrUtil.isNotBlank(request.getName()), AlertTemplateEntity::getName, request != null ? request.getName() : null)
                .eq(request != null && StrUtil.isNotBlank(request.getStatus()), AlertTemplateEntity::getStatus, request != null ? request.getStatus() : null)
        );
        return BeanUtil.copyToList(entityList, AlertTemplateRsp.class);
    }

    @Override
    public AlertTemplateRsp get(Long id) {
        return BeanUtil.copyProperties(this.getById(id), AlertTemplateRsp.class);
    }

    @Override
    public void save(AlertTemplateReq dto) {
        AlertTemplateEntity entity = BeanUtil.copyProperties(dto, AlertTemplateEntity.class);
        this.save(entity);
        BeanUtil.copyProperties(entity, dto);
    }

    @Override
    public void update(AlertTemplateReq dto) {
        this.updateById(BeanUtil.copyProperties(dto, AlertTemplateEntity.class));
    }

    @Override
    public void delete(Long[] ids) {
        AssertUtils.isArrayEmpty(ids, "id");
        this.removeByIds(Arrays.asList(ids));
    }

    @Override
    public Map<String, Object> preview(AlertTemplatePreviewReq req) {
        if (req == null || StrUtil.isBlank(req.getRawJson())) {
            throw new ServiceException("原始JSON不能为空");
        }
        AlertTemplateRsp template = this.get(req.getTemplateId());
        if (template == null) {
            throw new ServiceException("模板不存在");
        }
        Map<String, Object> payload = AlertJsonUtils.parsePayload(req.getRawJson());
        List<Map<String, Object>> alerts = AlertPayloadUtils.getAlerts(payload);
        Map<String, Object> alert = CollUtil.isNotEmpty(alerts) ? alerts.get(0) : new HashMap<>();
        Map<String, Object> context = AlertPayloadUtils.buildContext(payload, alert, null);

        Map<String, Object> result = new HashMap<>();
        result.put("subject", AlertTemplateRenderer.render(template.getEmailSubject(), context));
        result.put("html", AlertTemplateRenderer.render(template.getEmailHtml(), context));
        return result;
    }
}
