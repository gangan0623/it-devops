package net.leoch.modules.alert.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import net.leoch.common.exception.ServiceException;
import net.leoch.common.data.page.PageData;
import cn.hutool.core.bean.BeanUtil;
import net.leoch.common.data.validator.AssertUtils;
import net.leoch.modules.alert.mapper.AlertMediaMapper;
import net.leoch.modules.alert.vo.rsp.AlertMediaRsp;
import net.leoch.modules.alert.vo.req.AlertMediaPageReq;
import net.leoch.modules.alert.vo.req.AlertMediaReq;
import net.leoch.modules.alert.vo.req.AlertMediaTestReq;
import lombok.extern.slf4j.Slf4j;
import net.leoch.modules.alert.entity.AlertMediaEntity;
import net.leoch.modules.alert.service.AlertMailService;
import net.leoch.modules.alert.service.IAlertMediaService;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 告警媒介
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class AlertMediaServiceImpl extends ServiceImpl<AlertMediaMapper, AlertMediaEntity> implements IAlertMediaService {

    private final AlertMailService alertMailService;

    @Override
    public PageData<AlertMediaRsp> page(AlertMediaPageReq request) {
        IPage<AlertMediaEntity> page = this.page(request.buildPage(),
            new LambdaQueryWrapper<AlertMediaEntity>()
                .like(StrUtil.isNotBlank(request.getName()), AlertMediaEntity::getName, request.getName())
                .eq(StrUtil.isNotBlank(request.getStatus()), AlertMediaEntity::getStatus, request.getStatus())
        );
        return new PageData<>(BeanUtil.copyToList(page.getRecords(), AlertMediaRsp.class), page.getTotal());
    }

    @Override
    public List<AlertMediaRsp> list(AlertMediaPageReq request) {
        List<AlertMediaEntity> entityList = this.list(
            new LambdaQueryWrapper<AlertMediaEntity>()
                .like(request != null && StrUtil.isNotBlank(request.getName()), AlertMediaEntity::getName, request != null ? request.getName() : null)
                .eq(request != null && StrUtil.isNotBlank(request.getStatus()), AlertMediaEntity::getStatus, request != null ? request.getStatus() : null)
        );
        return BeanUtil.copyToList(entityList, AlertMediaRsp.class);
    }

    @Override
    public AlertMediaRsp get(Long id) {
        return BeanUtil.copyProperties(this.getById(id), AlertMediaRsp.class);
    }

    @Override
    public void save(AlertMediaReq dto) {
        AlertMediaEntity entity = BeanUtil.copyProperties(dto, AlertMediaEntity.class);
        this.save(entity);
        BeanUtils.copyProperties(entity, dto);
    }

    @Override
    public void update(AlertMediaReq dto) {
        this.updateById(BeanUtil.copyProperties(dto, AlertMediaEntity.class));
    }

    @Override
    public void delete(Long[] ids) {
        AssertUtils.isArrayEmpty(ids, "id");
        this.removeByIds(Arrays.asList(ids));
    }

    @Override
    public void testMedia(AlertMediaTestReq request) {
        log.info("[告警媒介] 测试请求, mediaId={}, to={}", request.getMediaId(), request.getTo());

        // 参数校验
        if (StrUtil.isBlank(request.getTo())) {
            throw new ServiceException("收件人不能为空");
        }

        // 查询媒介
        AlertMediaRsp mediaRsp = this.get(request.getMediaId());
        if (mediaRsp == null) {
            throw new ServiceException("媒介不存在");
        }

        // 数据转换
        AlertMediaEntity media = new AlertMediaEntity();
        media.setHost(mediaRsp.getHost());
        media.setPort(mediaRsp.getPort());
        media.setUsername(mediaRsp.getUsername());
        media.setPassword(mediaRsp.getPassword());
        media.setProtocol(mediaRsp.getProtocol());
        media.setSmtpAuth(mediaRsp.getSmtpAuth());
        media.setStarttlsEnable(mediaRsp.getStarttlsEnable());
        media.setTlsEnable(mediaRsp.getTlsEnable());
        media.setFromAddr(mediaRsp.getFromAddr());

        // 解析收件人
        List<String> receivers = StrUtil.split(request.getTo(), ',').stream()
            .map(String::trim)
            .filter(StrUtil::isNotBlank)
            .collect(Collectors.toList());

        // 发送测试邮件
        alertMailService.send(media, receivers, request.getSubject(), request.getContent(), request.getHtml());
        log.info("[告警媒介] 测试邮件发送成功, mediaId={}, receivers={}", request.getMediaId(), receivers);
    }
}
