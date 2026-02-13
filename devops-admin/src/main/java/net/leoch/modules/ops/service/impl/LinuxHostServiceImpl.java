package net.leoch.modules.ops.service.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.alibaba.excel.EasyExcel;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.data.page.PageData;
import net.leoch.common.data.validator.ValidatorUtils;
import net.leoch.common.data.validator.group.AddGroup;
import net.leoch.common.data.validator.group.DefaultGroup;
import net.leoch.common.data.validator.group.UpdateGroup;
import net.leoch.common.exception.ServiceException;
import net.leoch.common.integration.excel.LinuxHostExcel;
import net.leoch.common.integration.excel.template.LinuxHostImportExcel;
import net.leoch.common.integration.security.SecurityUser;
import net.leoch.common.utils.excel.ExcelUtils;
import net.leoch.common.utils.ops.MetricsUtils;
import net.leoch.common.utils.ops.OpsQueryUtils;
import net.leoch.common.utils.redis.RedisKeys;
import net.leoch.common.utils.redis.RedisUtils;
import net.leoch.modules.ops.entity.LinuxHostEntity;
import net.leoch.modules.ops.mapper.LinuxHostMapper;
import net.leoch.modules.ops.service.ILinuxHostService;
import net.leoch.modules.ops.vo.req.*;
import net.leoch.modules.ops.vo.rsp.LinuxHostRsp;
import net.leoch.modules.ops.vo.rsp.OpsHostStatusSummaryRsp;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

/**
 * Linux主机表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class LinuxHostServiceImpl extends ServiceImpl<LinuxHostMapper, LinuxHostEntity> implements ILinuxHostService {

    private final RedisUtils redisUtils;

    @Override
    public PageData<LinuxHostRsp> page(LinuxHostPageReq request) {
        LambdaQueryWrapper<LinuxHostEntity> wrapper = new LambdaQueryWrapper<>();
        applyCommonFilters(wrapper, request);
        if ("online_status".equalsIgnoreCase(request.getOrderField())) {
            List<LinuxHostEntity> list = this.list(wrapper);
            List<LinuxHostRsp> dtoList = BeanUtil.copyToList(list, LinuxHostRsp.class);
            fillOnlineStatus(dtoList);
            OnlineStatusSupport.sortByOnlineStatus(dtoList, request.getOrder(), LinuxHostRsp::getOnlineStatus);
            return OnlineStatusSupport.buildPageData(dtoList, request.getPage(), request.getLimit());
        }
        Page<LinuxHostEntity> page = request.buildPage();
        IPage<LinuxHostEntity> result = this.page(page, wrapper);
        List<LinuxHostRsp> dtoList = BeanUtil.copyToList(result.getRecords(), LinuxHostRsp.class);
        fillOnlineStatus(dtoList);
        return new PageData<>(dtoList, result.getTotal());
    }

    @Override
    public LinuxHostRsp get(LinuxHostIdReq request) {
        if (request == null || request.getId() == null) {
            return null;
        }
        LinuxHostEntity entity = this.getById(request.getId());
        LinuxHostRsp dto = BeanUtil.copyProperties(entity, LinuxHostRsp.class);
        if (dto != null) {
            fillOnlineStatus(Collections.singletonList(dto));
        }
        return dto;
    }

    @Override
    public void save(LinuxHostSaveReq dto) {
        log.info("[LinuxHost] 开始保存, instance={}", dto != null ? dto.getInstance() : null);
        ValidatorUtils.validateEntity(dto, AddGroup.class, DefaultGroup.class);
        validateUnique(dto.getInstance(), dto.getName(), dto.getId());
        LinuxHostEntity entity = BeanUtil.copyProperties(dto, LinuxHostEntity.class);
        this.save(entity);
        BeanUtil.copyProperties(entity, dto);
    }

    @Override
    public void update(LinuxHostUpdateReq dto) {
        log.info("[LinuxHost] 开始更新, id={}", dto != null ? dto.getId() : null);
        ValidatorUtils.validateEntity(dto, UpdateGroup.class, DefaultGroup.class);
        validateUnique(dto.getInstance(), dto.getName(), dto.getId());
        LinuxHostEntity entity = BeanUtil.copyProperties(dto, LinuxHostEntity.class);
        this.updateById(entity);
    }

    @Override
    public void updateStatus(LinuxHostStatusUpdateReq request) {
        if (request == null) {
            return;
        }
        updateStatus(request.getIds(), request.getStatus());
    }

    @Override
    public boolean online(LinuxHostOnlineReq request) {
        if (request == null || StrUtil.isBlank(request.getInstance())) {
            return false;
        }
        return MetricsUtils.metricsOk(request.getInstance(), 3000);
    }

    @Override
    public OpsHostStatusSummaryRsp summary(LinuxHostPageReq request) {
        LambdaQueryWrapper<LinuxHostEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.select(LinuxHostEntity::getInstance, LinuxHostEntity::getStatus);
        List<LinuxHostEntity> list = this.list(wrapper);
        Map<String, Object> statusMap = redisUtils.hGetAll(RedisKeys.getLinuxHostOnlineKey());
        OpsHostStatusSummaryRsp summary = new OpsHostStatusSummaryRsp();
        summary.setTotalCount((long) list.size());
        for (LinuxHostEntity item : list) {
            if (item == null) {
                continue;
            }
            Integer status = item.getStatus();
            if (Integer.valueOf(1).equals(status)) {
                summary.setEnabledCount(summary.getEnabledCount() + 1);
            } else if (Integer.valueOf(0).equals(status)) {
                summary.setDisabledCount(summary.getDisabledCount() + 1);
            }
            Boolean onlineStatus = OnlineStatusSupport.resolveOnlineStatus(statusMap == null ? null : statusMap.get(item.getInstance()));
            if (Boolean.TRUE.equals(onlineStatus)) {
                summary.setOnlineCount(summary.getOnlineCount() + 1);
            } else if (Boolean.FALSE.equals(onlineStatus)) {
                summary.setOfflineCount(summary.getOfflineCount() + 1);
            } else {
                summary.setUnknownCount(summary.getUnknownCount() + 1);
            }
        }
        return summary;
    }

    @Override
    public boolean check(LinuxHostCheckReq request) {
        if (request == null) {
            return false;
        }
        return existsByInstanceOrName(request.getInstance(), request.getName(), request.getId());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void importExcel(LinuxHostImportReq request) throws Exception {
        if (request == null || request.getFile() == null || request.getFile().isEmpty()) {
            throw new ServiceException("上传文件不能为空");
        }
        List<LinuxHostImportExcel> dataList = EasyExcel.read(request.getFile().getInputStream()).head(LinuxHostImportExcel.class).sheet().doReadSync();
        if (CollUtil.isEmpty(dataList)) {
            throw new ServiceException("导入数据不能为空");
        }
        List<LinuxHostEntity> entityList = new ArrayList<>(dataList.size());
        for (LinuxHostImportExcel item : dataList) {
            entityList.add(toEntity(item));
        }

        // 分批处理，避免大批量数据导致 SQL 超时或 OOM
        final int BATCH_SIZE = 1000;
        if (entityList.size() > BATCH_SIZE) {
            log.info("[Linux 主机] Excel 导入分批处理, 总数={}, 批次大小={}", entityList.size(), BATCH_SIZE);
            List<List<LinuxHostEntity>> batches = CollUtil.split(entityList, BATCH_SIZE);
            for (int i = 0; i < batches.size(); i++) {
                log.debug("[Linux 主机] 处理第 {}/{} 批, 数量={}", i + 1, batches.size(), batches.get(i).size());
                this.saveBatch(batches.get(i));
            }
        } else {
            this.saveBatch(entityList);
        }
        log.info("[Linux 主机] Excel 导入完成, 总数={}", entityList.size());
    }


    private void fillOnlineStatus(List<LinuxHostRsp> list) {
        if (list == null || list.isEmpty()) {
            return;
        }
        Map<String, Object> statusMap = redisUtils.hGetAll(RedisKeys.getLinuxHostOnlineKey());
        for (LinuxHostRsp dto : list) {
            String instance = dto.getInstance();
            dto.setOnlineStatus(OnlineStatusSupport.resolveOnlineStatus(statusMap == null ? null : statusMap.get(instance)));
        }
    }

    private void applyCommonFilters(LambdaQueryWrapper<LinuxHostEntity> wrapper, LinuxHostPageReq request) {
        wrapper.like(StrUtil.isNotBlank(request.getInstance()), LinuxHostEntity::getInstance, request.getInstance());
        wrapper.like(StrUtil.isNotBlank(request.getName()), LinuxHostEntity::getName, request.getName());
        wrapper.eq(StrUtil.isNotBlank(request.getSiteLocation()), LinuxHostEntity::getSiteLocation, request.getSiteLocation());
        wrapper.eq(StrUtil.isNotBlank(request.getAreaName()), LinuxHostEntity::getAreaName, request.getAreaName());
        wrapper.eq(StrUtil.isNotBlank(request.getStatus()), LinuxHostEntity::getStatus, request.getStatus());
        wrapper.eq(StrUtil.isNotBlank(request.getMenuName()), LinuxHostEntity::getMenuName, request.getMenuName());
        wrapper.eq(StrUtil.isNotBlank(request.getType()), LinuxHostEntity::getType, request.getType());
    }

    private LinuxHostEntity toEntity(LinuxHostImportExcel item) {
        LinuxHostEntity entity = new LinuxHostEntity();
        entity.setInstance(item.getInstance());
        entity.setName(item.getName());
        entity.setAreaName(item.getAreaName());
        entity.setSiteLocation(item.getSiteLocation());
        entity.setMenuName(item.getMenuName());
        entity.setSubMenuName(item.getSubMenuName());
        entity.setType(item.getType());
        entity.setStatus(item.getStatus());
        return entity;
    }

    @Override
    public void template(HttpServletResponse response) throws Exception {
        ExcelUtils.exportExcel(response, "Linux主机导入模板", "Linux主机导入模板", new ArrayList<>(), LinuxHostImportExcel.class);
    }

    @Override
    public void export(LinuxHostPageReq request, HttpServletResponse response) throws Exception {
        LambdaQueryWrapper<LinuxHostEntity> wrapper = new LambdaQueryWrapper<>();
        applyCommonFilters(wrapper, request);
        List<LinuxHostEntity> list = this.list(wrapper);
        List<LinuxHostRsp> dtoList = BeanUtil.copyToList(list, LinuxHostRsp.class);
        ExcelUtils.exportExcelToTarget(response, null, "Linux主机表", dtoList, LinuxHostExcel.class);
    }

    @Override
    public void delete(LinuxHostDeleteReq request) {
        if (request == null || request.getIds() == null || request.getIds().length == 0) {
            return;
        }
        this.removeByIds(Arrays.asList(request.getIds()));
    }

    @Override
    public boolean existsByInstanceOrName(String instance, String name, Long excludeId) {
        return OpsQueryUtils.existsByInstanceOrName(
                this.getBaseMapper(),
                LinuxHostEntity::getId,
                LinuxHostEntity::getInstance,
                LinuxHostEntity::getName,
                instance,
                name,
                excludeId
        );
    }

    @Override
    public void updateStatus(Long[] ids, Integer status) {
        if (ids == null || ids.length == 0) {
            return;
        }
        LinuxHostEntity entity = new LinuxHostEntity();
        entity.setStatus(status);
        entity.setUpdater(SecurityUser.getUserId());
        entity.setUpdateDate(new Date());
        LambdaUpdateWrapper<LinuxHostEntity> wrapper = new LambdaUpdateWrapper<>();
        wrapper.in(LinuxHostEntity::getId, Arrays.asList(ids));
        this.update(entity, wrapper);
    }

    private void validateUnique(String instance, String name, Long excludeId) {
        if (existsByInstanceOrName(instance, name, excludeId)) {
            throw new ServiceException("地址或名称已存在");
        }
    }
}
