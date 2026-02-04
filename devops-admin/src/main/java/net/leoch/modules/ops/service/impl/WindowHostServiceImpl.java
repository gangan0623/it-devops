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
import net.leoch.common.validator.ValidatorUtils;
import net.leoch.common.validator.group.AddGroup;
import net.leoch.common.validator.group.DefaultGroup;
import net.leoch.common.validator.group.UpdateGroup;
import net.leoch.modules.ops.dao.WindowHostDao;
import net.leoch.modules.ops.dto.*;
import net.leoch.modules.ops.entity.WindowHostEntity;
import net.leoch.modules.ops.excel.WindowHostExcel;
import net.leoch.modules.ops.excel.template.WindowHostImportExcel;
import net.leoch.modules.ops.service.WindowHostService;
import net.leoch.modules.ops.util.MetricsUtils;
import net.leoch.modules.ops.util.OpsQueryUtils;
import net.leoch.modules.security.user.SecurityUser;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

/**
 * Windows主机表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Service
public class WindowHostServiceImpl extends CrudServiceImpl<WindowHostDao, WindowHostEntity, WindowHostDTO> implements WindowHostService {

    @Resource
    private RedisUtils redisUtils;

    @Override
    public QueryWrapper<WindowHostEntity> getWrapper(Map<String, Object> params) {
        QueryWrapper<WindowHostEntity> wrapper = new QueryWrapper<>();
        LambdaQueryWrapper<WindowHostEntity> lambda = wrapper.lambda();
        String id = (String) params.get("id");
        String instance = (String) params.get("instance");
        String name = (String) params.get("name");
        String areaName = (String) params.get("areaName");
        String siteLocation = (String) params.get("siteLocation");
        String menuName = (String) params.get("menuName");
        String type = (String) params.get("type");
        String status = (String) params.get("status");
        lambda.eq(StrUtil.isNotBlank(id), WindowHostEntity::getId, id);
        WindowHostPageRequest req = new WindowHostPageRequest();
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
    public PageData<WindowHostDTO> page(WindowHostPageRequest request) {
        LambdaQueryWrapper<WindowHostEntity> wrapper = new LambdaQueryWrapper<>();
        applyCommonFilters(wrapper, request);
        if ("online_status".equalsIgnoreCase(request.getOrderField())) {
            List<WindowHostEntity> list = baseDao.selectList(wrapper);
            List<WindowHostDTO> dtoList = ConvertUtils.sourceToTarget(list, WindowHostDTO.class);
            fillOnlineStatus(dtoList);
            OnlineStatusSupport.sortByOnlineStatus(dtoList, request.getOrder(), WindowHostDTO::getOnlineStatus);
            return OnlineStatusSupport.buildPageData(dtoList, request.getPage(), request.getLimit());
        }
        Page<WindowHostEntity> page = buildPage(request);
        IPage<WindowHostEntity> result = baseDao.selectPage(page, wrapper);
        List<WindowHostDTO> dtoList = ConvertUtils.sourceToTarget(result.getRecords(), WindowHostDTO.class);
        fillOnlineStatus(dtoList);
        return new PageData<>(dtoList, result.getTotal());
    }

    @Override
    public WindowHostDTO get(WindowHostIdRequest request) {
        if (request == null || request.getId() == null) {
            return null;
        }
        WindowHostEntity entity = baseDao.selectById(request.getId());
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
        super.save(dto);
    }

    @Override
    public void update(WindowHostDTO dto) {
        ValidatorUtils.validateEntity(dto, UpdateGroup.class, DefaultGroup.class);
        validateUnique(dto);
        super.update(dto);
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
    public boolean check(WindowHostCheckRequest request) {
        if (request == null) {
            return false;
        }
        return existsByInstanceOrName(request.getInstance(), request.getName(), request.getId());
    }

    @Override
    @Transactional
    public void importExcel(WindowHostImportRequest request) throws Exception {
        if (request == null || request.getFile() == null || request.getFile().isEmpty()) {
            throw new RenException("上传文件不能为空");
        }
        List<WindowHostImportExcel> dataList = EasyExcel.read(request.getFile().getInputStream()).head(WindowHostImportExcel.class).sheet().doReadSync();
        if (CollUtil.isEmpty(dataList)) {
            throw new RenException("导入数据不能为空");
        }
        List<WindowHostEntity> entityList = new ArrayList<>(dataList.size());
        for (WindowHostImportExcel item : dataList) {
            entityList.add(toEntity(item));
        }
        insertBatch(entityList);
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
        List<WindowHostEntity> list = baseDao.selectList(wrapper);
        List<WindowHostDTO> dtoList = ConvertUtils.sourceToTarget(list, WindowHostDTO.class);
        ExcelUtils.exportExcelToTarget(response, null, "Windows主机表", dtoList, WindowHostExcel.class);
    }

    @Override
    public void delete(WindowHostDeleteRequest request) {
        if (request == null || request.getIds() == null || request.getIds().length == 0) {
            return;
        }
        super.delete(request.getIds());
    }

    @Override
    public boolean existsByInstanceOrName(String instance, String name, Long excludeId) {
        return OpsQueryUtils.existsByInstanceOrName(
                baseDao,
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
        baseDao.update(entity, wrapper);
    }

    private Page<WindowHostEntity> buildPage(WindowHostPageRequest request) {
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

    private void validateUnique(WindowHostDTO dto) {
        if (dto != null && existsByInstanceOrName(dto.getInstance(), dto.getName(), dto.getId())) {
            throw new RenException("地址或名称已存在");
        }
    }

}
