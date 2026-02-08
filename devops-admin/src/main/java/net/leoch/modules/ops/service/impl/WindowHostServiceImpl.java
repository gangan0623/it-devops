package net.leoch.modules.ops.service.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.alibaba.excel.EasyExcel;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import jakarta.annotation.Resource;
import jakarta.servlet.http.HttpServletResponse;
import net.leoch.common.exception.ServiceException;
import net.leoch.common.page.PageData;
import net.leoch.common.redis.RedisKeys;
import net.leoch.common.redis.RedisUtils;
import net.leoch.common.utils.ConvertUtils;
import net.leoch.common.utils.ExcelUtils;
import net.leoch.common.validator.ValidatorUtils;
import net.leoch.common.validator.group.AddGroup;
import net.leoch.common.validator.group.DefaultGroup;
import net.leoch.common.validator.group.UpdateGroup;
import net.leoch.modules.ops.mapper.WindowHostMapper;
import net.leoch.modules.ops.dto.*;
import net.leoch.modules.ops.entity.WindowHostEntity;
import net.leoch.modules.ops.excel.WindowHostExcel;
import net.leoch.modules.ops.excel.template.WindowHostImportExcel;
import net.leoch.modules.ops.service.IWindowHostService;
import net.leoch.modules.ops.util.MetricsUtils;
import net.leoch.modules.ops.util.OpsQueryUtils;
import net.leoch.modules.security.user.SecurityUser;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

/**
 * Windows主机表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Slf4j
@Service
public class WindowHostServiceImpl extends ServiceImpl<WindowHostMapper, WindowHostEntity> implements IWindowHostService {

    @Resource
    private RedisUtils redisUtils;

    @Override
    public PageData<WindowHostDTO> page(WindowHostPageRequest request) {
        LambdaQueryWrapper<WindowHostEntity> wrapper = new LambdaQueryWrapper<>();
        applyCommonFilters(wrapper, request);
        if ("online_status".equalsIgnoreCase(request.getOrderField())) {
            List<WindowHostEntity> list = this.list(wrapper);
            List<WindowHostDTO> dtoList = ConvertUtils.sourceToTarget(list, WindowHostDTO.class);
            fillOnlineStatus(dtoList);
            OnlineStatusSupport.sortByOnlineStatus(dtoList, request.getOrder(), WindowHostDTO::getOnlineStatus);
            return OnlineStatusSupport.buildPageData(dtoList, request.getPage(), request.getLimit());
        }
        Page<WindowHostEntity> page = request.buildPage();
        IPage<WindowHostEntity> result = this.page(page, wrapper);
        List<WindowHostDTO> dtoList = ConvertUtils.sourceToTarget(result.getRecords(), WindowHostDTO.class);
        fillOnlineStatus(dtoList);
        return new PageData<>(dtoList, result.getTotal());
    }

    @Override
    public WindowHostDTO get(WindowHostIdRequest request) {
        if (request == null || request.getId() == null) {
            return null;
        }
        WindowHostEntity entity = this.getById(request.getId());
        WindowHostDTO dto = ConvertUtils.sourceToTarget(entity, WindowHostDTO.class);
        if (dto != null) {
            fillOnlineStatus(Collections.singletonList(dto));
        }
        return dto;
    }

    @Override
    public void save(WindowHostDTO dto) {
        ValidatorUtils.validateEntity(dto, AddGroup.class, DefaultGroup.class);
        validateUnique(dto);
        WindowHostEntity entity = ConvertUtils.sourceToTarget(dto, WindowHostEntity.class);
        this.save(entity);
        BeanUtils.copyProperties(entity, dto);
    }

    @Override
    public void update(WindowHostDTO dto) {
        ValidatorUtils.validateEntity(dto, UpdateGroup.class, DefaultGroup.class);
        validateUnique(dto);
        WindowHostEntity entity = ConvertUtils.sourceToTarget(dto, WindowHostEntity.class);
        this.updateById(entity);
    }

    @Override
    public void updateStatus(WindowHostStatusUpdateRequest request) {
        if (request == null) {
            return;
        }
        updateStatus(request.getIds(), request.getStatus());
    }

    @Override
    public boolean online(WindowHostOnlineRequest request) {
        if (request == null || StrUtil.isBlank(request.getInstance())) {
            return false;
        }
        return MetricsUtils.metricsOk(request.getInstance(), 3000);
    }

    @Override
    public OpsHostStatusSummaryDTO summary(WindowHostPageRequest request) {
        LambdaQueryWrapper<WindowHostEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.select(WindowHostEntity::getInstance, WindowHostEntity::getStatus);
        List<WindowHostEntity> list = this.list(wrapper);
        Map<String, Object> statusMap = redisUtils.hGetAll(RedisKeys.getWindowHostOnlineKey());
        OpsHostStatusSummaryDTO summary = new OpsHostStatusSummaryDTO();
        summary.setTotalCount((long) list.size());
        for (WindowHostEntity item : list) {
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
    public boolean check(WindowHostCheckRequest request) {
        if (request == null) {
            return false;
        }
        return existsByInstanceOrName(request.getInstance(), request.getName(), request.getId());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void importExcel(WindowHostImportRequest request) throws Exception {
        if (request == null || request.getFile() == null || request.getFile().isEmpty()) {
            throw new ServiceException("上传文件不能为空");
        }
        List<WindowHostImportExcel> dataList = EasyExcel.read(request.getFile().getInputStream()).head(WindowHostImportExcel.class).sheet().doReadSync();
        if (CollUtil.isEmpty(dataList)) {
            throw new ServiceException("导入数据不能为空");
        }
        List<WindowHostEntity> entityList = new ArrayList<>(dataList.size());
        for (WindowHostImportExcel item : dataList) {
            entityList.add(toEntity(item));
        }
        this.saveBatch(entityList);
    }


    private void fillOnlineStatus(List<WindowHostDTO> list) {
        if (list == null || list.isEmpty()) {
            return;
        }
        Map<String, Object> statusMap = redisUtils.hGetAll(RedisKeys.getWindowHostOnlineKey());
        for (WindowHostDTO dto : list) {
            String instance = dto.getInstance();
            dto.setOnlineStatus(OnlineStatusSupport.resolveOnlineStatus(statusMap == null ? null : statusMap.get(instance)));
        }
    }

    private void applyCommonFilters(LambdaQueryWrapper<WindowHostEntity> wrapper, WindowHostPageRequest request) {
        wrapper.like(StrUtil.isNotBlank(request.getInstance()), WindowHostEntity::getInstance, request.getInstance());
        wrapper.like(StrUtil.isNotBlank(request.getName()), WindowHostEntity::getName, request.getName());
        wrapper.eq(StrUtil.isNotBlank(request.getSiteLocation()), WindowHostEntity::getSiteLocation, request.getSiteLocation());
        wrapper.eq(StrUtil.isNotBlank(request.getAreaName()), WindowHostEntity::getAreaName, request.getAreaName());
        wrapper.eq(StrUtil.isNotBlank(request.getStatus()), WindowHostEntity::getStatus, request.getStatus());
        wrapper.eq(StrUtil.isNotBlank(request.getMenuName()), WindowHostEntity::getMenuName, request.getMenuName());
        wrapper.eq(StrUtil.isNotBlank(request.getType()), WindowHostEntity::getType, request.getType());
    }

    private WindowHostEntity toEntity(WindowHostImportExcel item) {
        WindowHostEntity entity = new WindowHostEntity();
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
        ExcelUtils.exportExcel(response, "Windows主机导入模板", "Windows主机导入模板", new ArrayList<>(), WindowHostImportExcel.class);
    }

    @Override
    public void export(WindowHostPageRequest request, HttpServletResponse response) throws Exception {
        LambdaQueryWrapper<WindowHostEntity> wrapper = new LambdaQueryWrapper<>();
        applyCommonFilters(wrapper, request);
        List<WindowHostEntity> list = this.list(wrapper);
        List<WindowHostDTO> dtoList = ConvertUtils.sourceToTarget(list, WindowHostDTO.class);
        ExcelUtils.exportExcelToTarget(response, null, "Windows主机表", dtoList, WindowHostExcel.class);
    }

    @Override
    public void delete(WindowHostDeleteRequest request) {
        if (request == null || request.getIds() == null || request.getIds().length == 0) {
            return;
        }
        this.removeByIds(Arrays.asList(request.getIds()));
    }

    @Override
    public boolean existsByInstanceOrName(String instance, String name, Long excludeId) {
        return OpsQueryUtils.existsByInstanceOrName(
                this.getBaseMapper(),
                WindowHostEntity::getId,
                WindowHostEntity::getInstance,
                WindowHostEntity::getName,
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
        WindowHostEntity entity = new WindowHostEntity();
        entity.setStatus(status);
        entity.setUpdater(SecurityUser.getUserId());
        entity.setUpdateDate(new Date());
        LambdaUpdateWrapper<WindowHostEntity> wrapper = new LambdaUpdateWrapper<>();
        wrapper.in(WindowHostEntity::getId, Arrays.asList(ids));
        this.update(entity, wrapper);
    }

    private void validateUnique(WindowHostDTO dto) {
        if (dto != null && existsByInstanceOrName(dto.getInstance(), dto.getName(), dto.getId())) {
            throw new ServiceException("地址或名称已存在");
        }
    }

}
