package net.leoch.modules.ops.service.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.alibaba.excel.EasyExcel;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import jakarta.annotation.Resource;
import jakarta.servlet.http.HttpServletResponse;
import net.leoch.common.exception.ServiceException;
import net.leoch.common.page.PageData;
import net.leoch.common.redis.RedisKeys;
import net.leoch.common.redis.RedisUtils;
import net.leoch.common.service.impl.CrudServiceImpl;
import net.leoch.common.utils.ConvertUtils;
import net.leoch.common.utils.ExcelUtils;
import net.leoch.common.validator.ValidatorUtils;
import net.leoch.common.validator.group.AddGroup;
import net.leoch.common.validator.group.DefaultGroup;
import net.leoch.common.validator.group.UpdateGroup;
import net.leoch.modules.ops.dao.LinuxHostDao;
import net.leoch.modules.ops.dto.*;
import net.leoch.modules.ops.entity.LinuxHostEntity;
import net.leoch.modules.ops.excel.LinuxHostExcel;
import net.leoch.modules.ops.excel.template.LinuxHostImportExcel;
import net.leoch.modules.ops.service.LinuxHostService;
import net.leoch.modules.ops.util.MetricsUtils;
import net.leoch.modules.ops.util.OpsQueryUtils;
import net.leoch.modules.security.user.SecurityUser;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

/**
 * Linux主机表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Service
public class LinuxHostServiceImpl extends CrudServiceImpl<LinuxHostDao, LinuxHostEntity, LinuxHostDTO> implements LinuxHostService {

    @Resource
    private RedisUtils redisUtils;

    @Override
    public QueryWrapper<LinuxHostEntity> getWrapper(Map<String, Object> params){
        QueryWrapper<LinuxHostEntity> wrapper = new QueryWrapper<>();
        LambdaQueryWrapper<LinuxHostEntity> lambda = wrapper.lambda();
        String id = (String)params.get("id");
        String instance = (String)params.get("instance");
        String name = (String)params.get("name");
        String areaName = (String) params.get("areaName");
        String siteLocation = (String) params.get("siteLocation");
        String menuName = (String) params.get("menuName");
        String type = (String) params.get("type");
        String status = (String) params.get("status");
        lambda.eq(StrUtil.isNotBlank(id), LinuxHostEntity::getId, id);
        LinuxHostPageRequest req = new LinuxHostPageRequest();
        req.setInstance(instance);
        req.setName(name);
        req.setAreaName(areaName);
        req.setSiteLocation(siteLocation);
        req.setMenuName(menuName);
        req.setType(type);
        req.setStatus(status);
        applyCommonFilters(lambda, req);
        return wrapper;
    }

    @Override
    public PageData<LinuxHostDTO> page(LinuxHostPageRequest request) {
        LambdaQueryWrapper<LinuxHostEntity> wrapper = new LambdaQueryWrapper<>();
        applyCommonFilters(wrapper, request);
        if ("online_status".equalsIgnoreCase(request.getOrderField())) {
            List<LinuxHostEntity> list = baseDao.selectList(wrapper);
            List<LinuxHostDTO> dtoList = ConvertUtils.sourceToTarget(list, LinuxHostDTO.class);
            fillOnlineStatus(dtoList);
            OnlineStatusSupport.sortByOnlineStatus(dtoList, request.getOrder(), LinuxHostDTO::getOnlineStatus);
            return OnlineStatusSupport.buildPageData(dtoList, request.getPage(), request.getLimit());
        }
        Page<LinuxHostEntity> page = buildPage(request);
        IPage<LinuxHostEntity> result = baseDao.selectPage(page, wrapper);
        List<LinuxHostDTO> dtoList = ConvertUtils.sourceToTarget(result.getRecords(), LinuxHostDTO.class);
        fillOnlineStatus(dtoList);
        return new PageData<>(dtoList, result.getTotal());
    }

    @Override
    public LinuxHostDTO get(LinuxHostIdRequest request) {
        if (request == null || request.getId() == null) {
            return null;
        }
        LinuxHostEntity entity = baseDao.selectById(request.getId());
        LinuxHostDTO dto = ConvertUtils.sourceToTarget(entity, LinuxHostDTO.class);
        if (dto != null) {
            fillOnlineStatus(Collections.singletonList(dto));
        }
        return dto;
    }

    @Override
    public void save(LinuxHostDTO dto) {
        ValidatorUtils.validateEntity(dto, AddGroup.class, DefaultGroup.class);
        validateUnique(dto);
        super.save(dto);
    }

    @Override
    public void update(LinuxHostDTO dto) {
        ValidatorUtils.validateEntity(dto, UpdateGroup.class, DefaultGroup.class);
        validateUnique(dto);
        super.update(dto);
    }

    @Override
    public void updateStatus(LinuxHostStatusUpdateRequest request) {
        if (request == null) {
            return;
        }
        updateStatus(request.getIds(), request.getStatus());
    }

    @Override
    public boolean online(LinuxHostOnlineRequest request) {
        if (request == null || StrUtil.isBlank(request.getInstance())) {
            return false;
        }
        return MetricsUtils.metricsOk(request.getInstance(), 3000);
    }

    @Override
    public OpsHostStatusSummaryDTO summary(LinuxHostPageRequest request) {
        LambdaQueryWrapper<LinuxHostEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.select(LinuxHostEntity::getInstance, LinuxHostEntity::getStatus);
        List<LinuxHostEntity> list = baseDao.selectList(wrapper);
        Map<String, Object> statusMap = redisUtils.hGetAll(RedisKeys.getLinuxHostOnlineKey());
        OpsHostStatusSummaryDTO summary = new OpsHostStatusSummaryDTO();
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
    public boolean check(LinuxHostCheckRequest request) {
        if (request == null) {
            return false;
        }
        return existsByInstanceOrName(request.getInstance(), request.getName(), request.getId());
    }

    @Override
    @Transactional
    public void importExcel(LinuxHostImportRequest request) throws Exception {
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
        insertBatch(entityList);
    }


    private void fillOnlineStatus(List<LinuxHostDTO> list) {
        if (list == null || list.isEmpty()) {
            return;
        }
        Map<String, Object> statusMap = redisUtils.hGetAll(RedisKeys.getLinuxHostOnlineKey());
        for (LinuxHostDTO dto : list) {
            String instance = dto.getInstance();
            dto.setOnlineStatus(OnlineStatusSupport.resolveOnlineStatus(statusMap == null ? null : statusMap.get(instance)));
        }
    }

    private void applyCommonFilters(LambdaQueryWrapper<LinuxHostEntity> wrapper, LinuxHostPageRequest request) {
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
    public void export(LinuxHostPageRequest request, HttpServletResponse response) throws Exception {
        LambdaQueryWrapper<LinuxHostEntity> wrapper = new LambdaQueryWrapper<>();
        applyCommonFilters(wrapper, request);
        List<LinuxHostEntity> list = baseDao.selectList(wrapper);
        List<LinuxHostDTO> dtoList = ConvertUtils.sourceToTarget(list, LinuxHostDTO.class);
        ExcelUtils.exportExcelToTarget(response, null, "Linux主机表", dtoList, LinuxHostExcel.class);
    }

    @Override
    public void delete(LinuxHostDeleteRequest request) {
        if (request == null || request.getIds() == null || request.getIds().length == 0) {
            return;
        }
        super.delete(request.getIds());
    }

    @Override
    public boolean existsByInstanceOrName(String instance, String name, Long excludeId) {
        return OpsQueryUtils.existsByInstanceOrName(
                baseDao,
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
        baseDao.update(entity, wrapper);
    }

    private Page<LinuxHostEntity> buildPage(LinuxHostPageRequest request) {
        if (request == null) {
            return new Page<>(1, 10);
        }
        return OpsQueryUtils.buildPage(
                request.getPage(),
                request.getLimit(),
                request.getOrderField(),
                request.getOrder()
        );
    }

    private void validateUnique(LinuxHostDTO dto) {
        if (dto != null && existsByInstanceOrName(dto.getInstance(), dto.getName(), dto.getId())) {
            throw new ServiceException("地址或名称已存在");
        }
    }
}
