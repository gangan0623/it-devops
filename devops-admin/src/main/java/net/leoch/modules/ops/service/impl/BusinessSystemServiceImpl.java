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
import net.leoch.common.exception.RenException;
import net.leoch.common.page.PageData;
import net.leoch.common.redis.RedisKeys;
import net.leoch.common.redis.RedisUtils;
import net.leoch.common.service.impl.CrudServiceImpl;
import net.leoch.common.utils.ConvertUtils;
import net.leoch.common.utils.ExcelUtils;
import net.leoch.common.utils.PingUtils;
import net.leoch.common.validator.ValidatorUtils;
import net.leoch.common.validator.group.AddGroup;
import net.leoch.common.validator.group.DefaultGroup;
import net.leoch.common.validator.group.UpdateGroup;
import net.leoch.modules.ops.dao.BusinessSystemDao;
import net.leoch.modules.ops.dto.*;
import net.leoch.modules.ops.entity.BusinessSystemEntity;
import net.leoch.modules.ops.excel.BusinessSystemExcel;
import net.leoch.modules.ops.excel.template.BusinessSystemImportExcel;
import net.leoch.modules.ops.service.BusinessSystemService;
import net.leoch.modules.ops.util.OpsQueryUtils;
import net.leoch.modules.security.user.SecurityUser;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

/**
 * 业务系统表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Service
public class BusinessSystemServiceImpl extends CrudServiceImpl<BusinessSystemDao, BusinessSystemEntity, BusinessSystemDTO> implements BusinessSystemService {

    @Resource
    private RedisUtils redisUtils;

    @Override
    public QueryWrapper<BusinessSystemEntity> getWrapper(Map<String, Object> params){
        QueryWrapper<BusinessSystemEntity> wrapper = new QueryWrapper<>();
        LambdaQueryWrapper<BusinessSystemEntity> lambda = wrapper.lambda();
        String id = (String)params.get("id");
        String instance = (String)params.get("instance");
        String name = (String)params.get("name");
        String areaName = (String) params.get("areaName");
        String siteLocation = (String) params.get("siteLocation");
        String menuName = (String) params.get("menuName");
        String status = (String) params.get("status");
        lambda.eq(StrUtil.isNotBlank(id), BusinessSystemEntity::getId, id);
        BusinessSystemPageRequest req = new BusinessSystemPageRequest();
        req.setInstance(instance);
        req.setName(name);
        req.setAreaName(areaName);
        req.setSiteLocation(siteLocation);
        req.setMenuName(menuName);
        req.setStatus(status);
        applyCommonFilters(lambda, req);
        return wrapper;
    }

    @Override
    public PageData<BusinessSystemDTO> page(BusinessSystemPageRequest request) {
        LambdaQueryWrapper<BusinessSystemEntity> wrapper = new LambdaQueryWrapper<>();
        applyCommonFilters(wrapper, request);
        if ("online_status".equalsIgnoreCase(request.getOrderField())) {
            List<BusinessSystemEntity> list = baseDao.selectList(wrapper);
            List<BusinessSystemDTO> dtoList = ConvertUtils.sourceToTarget(list, BusinessSystemDTO.class);
            fillOnlineStatus(dtoList);
            OnlineStatusSupport.sortByOnlineStatus(dtoList, request.getOrder(), BusinessSystemDTO::getOnlineStatus);
            return OnlineStatusSupport.buildPageData(dtoList, request.getPage(), request.getLimit());
        }
        Page<BusinessSystemEntity> page = buildPage(request);
        IPage<BusinessSystemEntity> result = baseDao.selectPage(page, wrapper);
        List<BusinessSystemDTO> dtoList = ConvertUtils.sourceToTarget(result.getRecords(), BusinessSystemDTO.class);
        fillOnlineStatus(dtoList);
        return new PageData<>(dtoList, result.getTotal());
    }

    @Override
    public BusinessSystemDTO get(BusinessSystemIdRequest request) {
        if (request == null || request.getId() == null) {
            return null;
        }
        BusinessSystemEntity entity = baseDao.selectById(request.getId());
        BusinessSystemDTO dto = ConvertUtils.sourceToTarget(entity, BusinessSystemDTO.class);
        if (dto != null) {
            fillOnlineStatus(Collections.singletonList(dto));
        }
        return dto;
    }

    @Override
    public void save(BusinessSystemDTO dto) {
        ValidatorUtils.validateEntity(dto, AddGroup.class, DefaultGroup.class);
        validateUnique(dto);
        super.save(dto);
    }

    @Override
    public void update(BusinessSystemDTO dto) {
        ValidatorUtils.validateEntity(dto, UpdateGroup.class, DefaultGroup.class);
        validateUnique(dto);
        super.update(dto);
    }

    @Override
    public void updateStatus(BusinessSystemStatusUpdateRequest request) {
        if (request == null) {
            return;
        }
        updateStatus(request.getIds(), request.getStatus());
    }

    @Override
    public boolean online(BusinessSystemOnlineRequest request) {
        if (request == null || StrUtil.isBlank(request.getInstance())) {
            return false;
        }
        return PingUtils.isReachable(request.getInstance(), 2000);
    }

    @Override
    public OpsHostStatusSummaryDTO summary(BusinessSystemPageRequest request) {
        LambdaQueryWrapper<BusinessSystemEntity> wrapper = new LambdaQueryWrapper<>();
        applyCommonFilters(wrapper, request);
        wrapper.select(BusinessSystemEntity::getInstance, BusinessSystemEntity::getStatus);
        List<BusinessSystemEntity> list = baseDao.selectList(wrapper);
        Map<String, Object> statusMap = redisUtils.hGetAll(RedisKeys.getBusinessSystemOnlineKey());
        OpsHostStatusSummaryDTO summary = new OpsHostStatusSummaryDTO();
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
    public boolean check(BusinessSystemCheckRequest request) {
        if (request == null) {
            return false;
        }
        return existsByInstanceOrName(request.getInstance(), request.getName(), request.getId());
    }

    @Override
    @Transactional
    public void importExcel(BusinessSystemImportRequest request) throws Exception {
        if (request == null || request.getFile() == null || request.getFile().isEmpty()) {
            throw new RenException("上传文件不能为空");
        }
        List<BusinessSystemImportExcel> dataList = EasyExcel.read(request.getFile().getInputStream()).head(BusinessSystemImportExcel.class).sheet().doReadSync();
        if (CollUtil.isEmpty(dataList)) {
            throw new RenException("导入数据不能为空");
        }
        List<BusinessSystemEntity> entityList = new ArrayList<>(dataList.size());
        for (BusinessSystemImportExcel item : dataList) {
            entityList.add(toEntity(item));
        }
        insertBatch(entityList);
    }

    @Override
    public void template(HttpServletResponse response) throws Exception {
        ExcelUtils.exportExcel(response, "业务系统导入模板", "业务系统导入模板", new ArrayList<>(), BusinessSystemImportExcel.class);
    }

    @Override
    public void export(BusinessSystemPageRequest request, HttpServletResponse response) throws Exception {
        LambdaQueryWrapper<BusinessSystemEntity> wrapper = new LambdaQueryWrapper<>();
        applyCommonFilters(wrapper, request);
        List<BusinessSystemEntity> list = baseDao.selectList(wrapper);
        List<BusinessSystemDTO> dtoList = ConvertUtils.sourceToTarget(list, BusinessSystemDTO.class);
        ExcelUtils.exportExcelToTarget(response, null, "业务系统表", dtoList, BusinessSystemExcel.class);
    }

    @Override
    public void delete(BusinessSystemDeleteRequest request) {
        if (request == null || request.getIds() == null || request.getIds().length == 0) {
            return;
        }
        super.delete(request.getIds());
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
        baseDao.update(entity, wrapper);
    }

    @Override
    public boolean existsByInstanceOrName(String instance, String name, Long excludeId) {
        return OpsQueryUtils.existsByInstanceOrName(
                baseDao,
                BusinessSystemEntity::getId,
                BusinessSystemEntity::getInstance,
                BusinessSystemEntity::getName,
                instance,
                name,
                excludeId
        );
    }

    private void fillOnlineStatus(List<BusinessSystemDTO> list) {
        if (list == null || list.isEmpty()) {
            return;
        }
        Map<String, Object> statusMap = redisUtils.hGetAll(RedisKeys.getBusinessSystemOnlineKey());
        for (BusinessSystemDTO dto : list) {
            String instance = dto.getInstance();
            dto.setOnlineStatus(OnlineStatusSupport.resolveOnlineStatus(statusMap == null ? null : statusMap.get(instance)));
        }
    }

    private void applyCommonFilters(LambdaQueryWrapper<BusinessSystemEntity> wrapper, BusinessSystemPageRequest request) {
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

    private Page<BusinessSystemEntity> buildPage(BusinessSystemPageRequest request) {
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

    private void validateUnique(BusinessSystemDTO dto) {
        if (dto != null && existsByInstanceOrName(dto.getInstance(), dto.getName(), dto.getId())) {
            throw new RenException("地址或名称已存在");
        }
    }
}
