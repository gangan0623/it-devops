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
import net.leoch.common.utils.PingUtils;
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
import net.leoch.modules.security.user.SecurityUser;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * Linux主机表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Service
public class LinuxHostServiceImpl extends CrudServiceImpl<LinuxHostDao, LinuxHostEntity, LinuxHostDTO> implements LinuxHostService {

    @Override
    public QueryWrapper<LinuxHostEntity> getWrapper(Map<String, Object> params){
        QueryWrapper<LinuxHostEntity> wrapper = new QueryWrapper<>();
        LambdaQueryWrapper<LinuxHostEntity> lambda = wrapper.lambda();
        String id = (String)params.get("id");
        String instance = (String)params.get("instance");
        String name = (String)params.get("name");
        String areaName = (String)params.get("areaName");
        lambda.eq(StrUtil.isNotBlank(id), LinuxHostEntity::getId, id);
        lambda.like(StrUtil.isNotBlank(instance), LinuxHostEntity::getInstance, instance);
        lambda.like(StrUtil.isNotBlank(name), LinuxHostEntity::getName, name);
        lambda.eq(StrUtil.isNotBlank(areaName), LinuxHostEntity::getAreaName, areaName);
        return wrapper;
    }

    @Override
    public PageData<LinuxHostDTO> page(LinuxHostPageRequest request) {
        LambdaQueryWrapper<LinuxHostEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(StrUtil.isNotBlank(request.getInstance()), LinuxHostEntity::getInstance, request.getInstance());
        wrapper.like(StrUtil.isNotBlank(request.getName()), LinuxHostEntity::getName, request.getName());
        wrapper.eq(StrUtil.isNotBlank(request.getAreaName()), LinuxHostEntity::getAreaName, request.getAreaName());
        Page<LinuxHostEntity> page = buildPage(request);
        IPage<LinuxHostEntity> result = baseDao.selectPage(page, wrapper);
        return new PageData<>(ConvertUtils.sourceToTarget(result.getRecords(), LinuxHostDTO.class), result.getTotal());
    }

    @Override
    public LinuxHostDTO get(LinuxHostIdRequest request) {
        if (request == null || request.getId() == null) {
            return null;
        }
        LinuxHostEntity entity = baseDao.selectById(request.getId());
        return ConvertUtils.sourceToTarget(entity, LinuxHostDTO.class);
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
        return PingUtils.isReachable(request.getInstance(), 2000);
    }

    @Override
    public boolean check(LinuxHostCheckRequest request) {
        if (request == null) {
            return false;
        }
        return existsByInstanceOrName(request.getInstance(), request.getName(), request.getId());
    }

    @Override
    public void importExcel(LinuxHostImportRequest request) throws Exception {
        if (request == null || request.getFile() == null || request.getFile().isEmpty()) {
            throw new RenException("上传文件不能为空");
        }
        List<LinuxHostImportExcel> dataList = EasyExcel.read(request.getFile().getInputStream()).head(LinuxHostImportExcel.class).sheet().doReadSync();
        if (CollUtil.isEmpty(dataList)) {
            throw new RenException("导入数据不能为空");
        }
        List<LinuxHostEntity> entityList = new ArrayList<>(dataList.size());
        for (LinuxHostImportExcel item : dataList) {
            LinuxHostEntity entity = new LinuxHostEntity();
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

    @Override
    public void template(HttpServletResponse response) throws Exception {
        ExcelUtils.exportExcel(response, "Linux主机导入模板", "Linux主机导入模板", new ArrayList<>(), LinuxHostImportExcel.class);
    }

    @Override
    public void export(LinuxHostPageRequest request, HttpServletResponse response) throws Exception {
        LambdaQueryWrapper<LinuxHostEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(StrUtil.isNotBlank(request.getInstance()), LinuxHostEntity::getInstance, request.getInstance());
        wrapper.like(StrUtil.isNotBlank(request.getName()), LinuxHostEntity::getName, request.getName());
        wrapper.eq(StrUtil.isNotBlank(request.getAreaName()), LinuxHostEntity::getAreaName, request.getAreaName());
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
        if (StrUtil.isBlank(instance) && StrUtil.isBlank(name)) {
            return false;
        }
        LambdaQueryWrapper<LinuxHostEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.and((query) -> {
            if (StrUtil.isNotBlank(instance)) {
                query.or().eq(LinuxHostEntity::getInstance, instance);
            }
            if (StrUtil.isNotBlank(name)) {
                query.or().eq(LinuxHostEntity::getName, name);
            }
        });
        if (excludeId != null) {
            wrapper.ne(LinuxHostEntity::getId, excludeId);
        }
        return baseDao.selectCount(wrapper) > 0;
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
        Page<LinuxHostEntity> page = new Page<>(curPage, limit);
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

    private void validateUnique(LinuxHostDTO dto) {
        if (dto != null && existsByInstanceOrName(dto.getInstance(), dto.getName(), dto.getId())) {
            throw new RenException("地址或名称已存在");
        }
    }
}
