package net.leoch.modules.ops.service.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.alibaba.excel.EasyExcel;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.metadata.OrderItem;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import jakarta.servlet.http.HttpServletResponse;
import net.leoch.common.constant.Constant;
import net.leoch.common.exception.RenException;
import net.leoch.common.page.PageData;
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
import net.leoch.modules.security.user.SecurityUser;
import org.springframework.stereotype.Service;

import java.net.HttpURLConnection;
import java.net.URL;
import java.util.*;

/**
 * Windows主机表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Service
public class WindowHostServiceImpl extends CrudServiceImpl<WindowHostDao, WindowHostEntity, WindowHostDTO> implements WindowHostService {

    @Override
    public QueryWrapper<WindowHostEntity> getWrapper(Map<String, Object> params) {
        QueryWrapper<WindowHostEntity> wrapper = new QueryWrapper<>();
        LambdaQueryWrapper<WindowHostEntity> lambda = wrapper.lambda();
        String id = (String) params.get("id");
        String instance = (String) params.get("instance");
        String name = (String) params.get("name");
        String areaName = (String) params.get("areaName");
        lambda.eq(StrUtil.isNotBlank(id), WindowHostEntity::getId, id);
        lambda.like(StrUtil.isNotBlank(instance), WindowHostEntity::getInstance, instance);
        lambda.like(StrUtil.isNotBlank(name), WindowHostEntity::getName, name);
        lambda.eq(StrUtil.isNotBlank(areaName), WindowHostEntity::getAreaName, areaName);
        return wrapper;
    }

    @Override
    public PageData<WindowHostDTO> page(WindowHostPageRequest request) {
        LambdaQueryWrapper<WindowHostEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(StrUtil.isNotBlank(request.getInstance()), WindowHostEntity::getInstance, request.getInstance());
        wrapper.like(StrUtil.isNotBlank(request.getName()), WindowHostEntity::getName, request.getName());
        wrapper.eq(StrUtil.isNotBlank(request.getAreaName()), WindowHostEntity::getAreaName, request.getAreaName());
        Page<WindowHostEntity> page = buildPage(request);
        IPage<WindowHostEntity> result = baseDao.selectPage(page, wrapper);
        return new PageData<>(ConvertUtils.sourceToTarget(result.getRecords(), WindowHostDTO.class), result.getTotal());
    }

    @Override
    public WindowHostDTO get(WindowHostIdRequest request) {
        if (request == null || request.getId() == null) {
            return null;
        }
        WindowHostEntity entity = baseDao.selectById(request.getId());
        return ConvertUtils.sourceToTarget(entity, WindowHostDTO.class);
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
        return metricsOk(request.getInstance());
    }

    @Override
    public boolean check(WindowHostCheckRequest request) {
        if (request == null) {
            return false;
        }
        return existsByInstanceOrName(request.getInstance(), request.getName(), request.getId());
    }

    @Override
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
            WindowHostEntity entity = new WindowHostEntity();
            entity.setInstance(item.getInstance());
            entity.setName(item.getName());
            entity.setAreaName(item.getAreaName());
            entity.setSiteLocation(item.getSiteLocation());
            entity.setMenuName(item.getMenuName());
            entity.setSubMenuName(item.getSubMenuName());
            entity.setStatus(item.getStatus());
            entityList.add(entity);
        }
        insertBatch(entityList);
    }

    private boolean metricsOk(String instance) {
        String url = buildMetricsUrl(instance);
        if (StrUtil.isBlank(url)) {
            return false;
        }
        HttpURLConnection connection = null;
        try {
            connection = (HttpURLConnection) new URL(url).openConnection();
            connection.setRequestMethod("GET");
            connection.setConnectTimeout(3000);
            connection.setReadTimeout(3000);
            return connection.getResponseCode() == 200;
        } catch (Exception ignore) {
            return false;
        } finally {
            if (connection != null) {
                connection.disconnect();
            }
        }
    }

    private String buildMetricsUrl(String instance) {
        String base = instance == null ? "" : instance.trim();
        if (StrUtil.isBlank(base)) {
            return null;
        }
        if (!base.startsWith("http://") && !base.startsWith("https://")) {
            base = "http://" + base;
        }
        if (base.contains("/metrics")) {
            return base;
        }
        if (base.endsWith("/")) {
            return base + "metrics";
        }
        return base + "/metrics";
    }

    @Override
    public void template(HttpServletResponse response) throws Exception {
        ExcelUtils.exportExcel(response, "Windows主机导入模板", "Windows主机导入模板", new ArrayList<>(), WindowHostImportExcel.class);
    }

    @Override
    public void export(WindowHostPageRequest request, HttpServletResponse response) throws Exception {
        LambdaQueryWrapper<WindowHostEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(StrUtil.isNotBlank(request.getInstance()), WindowHostEntity::getInstance, request.getInstance());
        wrapper.like(StrUtil.isNotBlank(request.getName()), WindowHostEntity::getName, request.getName());
        wrapper.eq(StrUtil.isNotBlank(request.getAreaName()), WindowHostEntity::getAreaName, request.getAreaName());
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
        if (StrUtil.isBlank(instance) && StrUtil.isBlank(name)) {
            return false;
        }
        LambdaQueryWrapper<WindowHostEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.and((query) -> {
            if (StrUtil.isNotBlank(instance)) {
                query.or().eq(WindowHostEntity::getInstance, instance);
            }
            if (StrUtil.isNotBlank(name)) {
                query.or().eq(WindowHostEntity::getName, name);
            }
        });
        if (excludeId != null) {
            wrapper.ne(WindowHostEntity::getId, excludeId);
        }
        return baseDao.selectCount(wrapper) > 0;
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
        long curPage = 1;
        long limit = 10;
        if (request != null) {
            if (StrUtil.isNotBlank(request.getPage())) {
                curPage = Long.parseLong(request.getPage());
            }
            if (StrUtil.isNotBlank(request.getLimit())) {
                limit = Long.parseLong(request.getLimit());
            }
        }
        Page<WindowHostEntity> page = new Page<>(curPage, limit);
        if (request == null) {
            return page;
        }
        if (StrUtil.isNotBlank(request.getOrderField()) && StrUtil.isNotBlank(request.getOrder())) {
            if (Constant.ASC.equalsIgnoreCase(request.getOrder())) {
                page.addOrder(OrderItem.asc(request.getOrderField()));
            } else {
                page.addOrder(OrderItem.desc(request.getOrderField()));
            }
        }
        return page;
    }

    private void validateUnique(WindowHostDTO dto) {
        if (dto != null && existsByInstanceOrName(dto.getInstance(), dto.getName(), dto.getId())) {
            throw new RenException("地址或名称已存在");
        }
    }

}
