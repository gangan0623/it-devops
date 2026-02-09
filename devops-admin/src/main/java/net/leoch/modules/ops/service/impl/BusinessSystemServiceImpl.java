package net.leoch.modules.ops.service.impl;

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
import net.leoch.common.exception.ServiceException;
import net.leoch.common.page.PageData;
import net.leoch.common.redis.RedisKeys;
import net.leoch.common.redis.RedisUtils;
import net.leoch.common.utils.ConvertUtils;
import net.leoch.common.utils.ExcelUtils;
import net.leoch.common.utils.PingUtils;
import net.leoch.common.validator.ValidatorUtils;
import net.leoch.common.validator.group.AddGroup;
import net.leoch.common.validator.group.DefaultGroup;
import net.leoch.common.validator.group.UpdateGroup;
import net.leoch.modules.ops.mapper.BusinessSystemMapper;
import net.leoch.modules.ops.vo.req.*;
import net.leoch.modules.ops.vo.rsp.*;
import net.leoch.modules.ops.entity.BusinessSystemEntity;
import net.leoch.modules.ops.excel.BusinessSystemExcel;
import net.leoch.modules.ops.excel.template.BusinessSystemImportExcel;
import net.leoch.modules.ops.service.IBusinessSystemService;
import net.leoch.modules.ops.util.OpsQueryUtils;
import net.leoch.modules.security.user.SecurityUser;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

/**
 * 业务系统表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class BusinessSystemServiceImpl extends ServiceImpl<BusinessSystemMapper, BusinessSystemEntity> implements IBusinessSystemService {

    private final RedisUtils redisUtils;

    @Override
    public PageData<BusinessSystemRsp> page(BusinessSystemPageReq request) {
        LambdaQueryWrapper<BusinessSystemEntity> wrapper = new LambdaQueryWrapper<>();
        applyCommonFilters(wrapper, request);
        if ("online_status".equalsIgnoreCase(request.getOrderField())) {
            List<BusinessSystemEntity> list = this.list(wrapper);
            List<BusinessSystemRsp> dtoList = ConvertUtils.sourceToTarget(list, BusinessSystemRsp.class);
            fillOnlineStatus(dtoList);
            OnlineStatusSupport.sortByOnlineStatus(dtoList, request.getOrder(), BusinessSystemRsp::getOnlineStatus);
            return OnlineStatusSupport.buildPageData(dtoList, request.getPage(), request.getLimit());
        }
        Page<BusinessSystemEntity> page = request.buildPage();
        IPage<BusinessSystemEntity> result = this.page(page, wrapper);
        List<BusinessSystemRsp> dtoList = ConvertUtils.sourceToTarget(result.getRecords(), BusinessSystemRsp.class);
        fillOnlineStatus(dtoList);
        return new PageData<>(dtoList, result.getTotal());
    }

    @Override
    public BusinessSystemRsp get(BusinessSystemIdReq request) {
        if (request == null || request.getId() == null) {
            return null;
        }
        BusinessSystemEntity entity = this.getById(request.getId());
        BusinessSystemRsp dto = ConvertUtils.sourceToTarget(entity, BusinessSystemRsp.class);
        if (dto != null) {
            fillOnlineStatus(Collections.singletonList(dto));
        }
        return dto;
    }

    @Override
    public void save(BusinessSystemSaveReq dto) {
        ValidatorUtils.validateEntity(dto, AddGroup.class, DefaultGroup.class);
        validateUnique(dto.getInstance(), dto.getName(), dto.getId());
        BusinessSystemEntity entity = ConvertUtils.sourceToTarget(dto, BusinessSystemEntity.class);
        this.save(entity);
        BeanUtils.copyProperties(entity, dto);
    }

    @Override
    public void update(BusinessSystemUpdateReq dto) {
        ValidatorUtils.validateEntity(dto, UpdateGroup.class, DefaultGroup.class);
        validateUnique(dto.getInstance(), dto.getName(), dto.getId());
        BusinessSystemEntity entity = ConvertUtils.sourceToTarget(dto, BusinessSystemEntity.class);
        this.updateById(entity);
    }

    @Override
    public void updateStatus(BusinessSystemStatusUpdateReq request) {
        if (request == null) {
            return;
        }
        updateStatus(request.getIds(), request.getStatus());
    }

    @Override
    public boolean online(BusinessSystemOnlineReq request) {
        if (request == null || StrUtil.isBlank(request.getInstance())) {
            return false;
        }
        return PingUtils.isReachable(request.getInstance(), 2000);
    }

    @Override
    public OpsHostStatusSummaryRsp summary(BusinessSystemPageReq request) {
        LambdaQueryWrapper<BusinessSystemEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.select(BusinessSystemEntity::getInstance, BusinessSystemEntity::getStatus);
        List<BusinessSystemEntity> list = this.list(wrapper);
        Map<String, Object> statusMap = redisUtils.hGetAll(RedisKeys.getBusinessSystemOnlineKey());
        OpsHostStatusSummaryRsp summary = new OpsHostStatusSummaryRsp();
        summary.setTotalCount((long) list.size());
        for (BusinessSystemEntity item : list) {
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
    public boolean check(BusinessSystemCheckReq request) {
        if (request == null) {
            return false;
        }
        return existsByInstanceOrName(request.getInstance(), request.getName(), request.getId());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void importExcel(BusinessSystemImportReq request) throws Exception {
        if (request == null || request.getFile() == null || request.getFile().isEmpty()) {
            throw new ServiceException("上传文件不能为空");
        }
        List<BusinessSystemImportExcel> dataList = EasyExcel.read(request.getFile().getInputStream()).head(BusinessSystemImportExcel.class).sheet().doReadSync();
        if (CollUtil.isEmpty(dataList)) {
            throw new ServiceException("导入数据不能为空");
        }
        List<BusinessSystemEntity> entityList = new ArrayList<>(dataList.size());
        for (BusinessSystemImportExcel item : dataList) {
            entityList.add(toEntity(item));
        }
        this.saveBatch(entityList);
    }

    @Override
    public void template(HttpServletResponse response) throws Exception {
        ExcelUtils.exportExcel(response, "业务系统导入模板", "业务系统导入模板", new ArrayList<>(), BusinessSystemImportExcel.class);
    }

    @Override
    public void export(BusinessSystemPageReq request, HttpServletResponse response) throws Exception {
        LambdaQueryWrapper<BusinessSystemEntity> wrapper = new LambdaQueryWrapper<>();
        applyCommonFilters(wrapper, request);
        List<BusinessSystemEntity> list = this.list(wrapper);
        List<BusinessSystemRsp> dtoList = ConvertUtils.sourceToTarget(list, BusinessSystemRsp.class);
        ExcelUtils.exportExcelToTarget(response, null, "业务系统表", dtoList, BusinessSystemExcel.class);
    }

    @Override
    public void delete(BusinessSystemDeleteReq request) {
        if (request == null || request.getIds() == null || request.getIds().length == 0) {
            return;
        }
        this.removeByIds(Arrays.asList(request.getIds()));
    }

    @Override
    public void updateStatus(Long[] ids, Integer status) {
        if (ids == null || ids.length == 0) {
            return;
        }
        BusinessSystemEntity entity = new BusinessSystemEntity();
        entity.setStatus(status);
        entity.setUpdater(SecurityUser.getUserId());
        entity.setUpdateDate(new Date());
        LambdaUpdateWrapper<BusinessSystemEntity> wrapper = new LambdaUpdateWrapper<>();
        wrapper.in(BusinessSystemEntity::getId, Arrays.asList(ids));
        this.update(entity, wrapper);
    }

    @Override
    public boolean existsByInstanceOrName(String instance, String name, Long excludeId) {
        return OpsQueryUtils.existsByInstanceOrName(
                this.getBaseMapper(),
                BusinessSystemEntity::getId,
                BusinessSystemEntity::getInstance,
                BusinessSystemEntity::getName,
                instance,
                name,
                excludeId
        );
    }

    private void fillOnlineStatus(List<BusinessSystemRsp> list) {
        if (list == null || list.isEmpty()) {
            return;
        }
        Map<String, Object> statusMap = redisUtils.hGetAll(RedisKeys.getBusinessSystemOnlineKey());
        for (BusinessSystemRsp dto : list) {
            String instance = dto.getInstance();
            dto.setOnlineStatus(OnlineStatusSupport.resolveOnlineStatus(statusMap == null ? null : statusMap.get(instance)));
        }
    }

    private void applyCommonFilters(LambdaQueryWrapper<BusinessSystemEntity> wrapper, BusinessSystemPageReq request) {
        wrapper.like(StrUtil.isNotBlank(request.getInstance()), BusinessSystemEntity::getInstance, request.getInstance());
        wrapper.like(StrUtil.isNotBlank(request.getName()), BusinessSystemEntity::getName, request.getName());
        wrapper.eq(StrUtil.isNotBlank(request.getSiteLocation()), BusinessSystemEntity::getSiteLocation, request.getSiteLocation());
        wrapper.eq(StrUtil.isNotBlank(request.getAreaName()), BusinessSystemEntity::getAreaName, request.getAreaName());
        wrapper.eq(StrUtil.isNotBlank(request.getStatus()), BusinessSystemEntity::getStatus, request.getStatus());
        wrapper.eq(StrUtil.isNotBlank(request.getMenuName()), BusinessSystemEntity::getMenuName, request.getMenuName());
    }

    private BusinessSystemEntity toEntity(BusinessSystemImportExcel item) {
        BusinessSystemEntity entity = new BusinessSystemEntity();
        entity.setInstance(item.getInstance());
        entity.setName(item.getName());
        entity.setAreaName(item.getAreaName());
        entity.setSiteLocation(item.getSiteLocation());
        entity.setMenuName(item.getMenuName());
        entity.setSubMenuName(item.getSubMenuName());
        entity.setStatus(item.getStatus());
        return entity;
    }

    private void validateUnique(String instance, String name, Long excludeId) {
        if (existsByInstanceOrName(instance, name, excludeId)) {
            throw new ServiceException("地址或名称已存在");
        }
    }
}
