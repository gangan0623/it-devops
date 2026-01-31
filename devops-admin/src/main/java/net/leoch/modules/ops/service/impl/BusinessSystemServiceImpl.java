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
import net.leoch.modules.ops.dao.BusinessSystemDao;
import net.leoch.modules.ops.dto.*;
import net.leoch.modules.ops.entity.BusinessSystemEntity;
import net.leoch.modules.ops.excel.BusinessSystemExcel;
import net.leoch.modules.ops.excel.template.BusinessSystemImportExcel;
import net.leoch.modules.ops.service.BusinessSystemService;
import net.leoch.modules.security.user.SecurityUser;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * 业务系统表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Service
public class BusinessSystemServiceImpl extends CrudServiceImpl<BusinessSystemDao, BusinessSystemEntity, BusinessSystemDTO> implements BusinessSystemService {

    @Override
    public QueryWrapper<BusinessSystemEntity> getWrapper(Map<String, Object> params){
        QueryWrapper<BusinessSystemEntity> wrapper = new QueryWrapper<>();
        LambdaQueryWrapper<BusinessSystemEntity> lambda = wrapper.lambda();
        String id = (String)params.get("id");
        String instance = (String)params.get("instance");
        String name = (String)params.get("name");
        String areaName = (String)params.get("areaName");
        lambda.eq(StrUtil.isNotBlank(id), BusinessSystemEntity::getId, id);
        lambda.like(StrUtil.isNotBlank(instance), BusinessSystemEntity::getInstance, instance);
        lambda.like(StrUtil.isNotBlank(name), BusinessSystemEntity::getName, name);
        lambda.eq(StrUtil.isNotBlank(areaName), BusinessSystemEntity::getAreaName, areaName);
        return wrapper;
    }

    @Override
    public PageData<BusinessSystemDTO> page(BusinessSystemPageRequest request) {
        LambdaQueryWrapper<BusinessSystemEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(StrUtil.isNotBlank(request.getInstance()), BusinessSystemEntity::getInstance, request.getInstance());
        wrapper.like(StrUtil.isNotBlank(request.getName()), BusinessSystemEntity::getName, request.getName());
        wrapper.eq(StrUtil.isNotBlank(request.getAreaName()), BusinessSystemEntity::getAreaName, request.getAreaName());
        Page<BusinessSystemEntity> page = buildPage(request);
        IPage<BusinessSystemEntity> result = baseDao.selectPage(page, wrapper);
        return new PageData<>(ConvertUtils.sourceToTarget(result.getRecords(), BusinessSystemDTO.class), result.getTotal());
    }

    @Override
    public BusinessSystemDTO get(BusinessSystemIdRequest request) {
        if (request == null || request.getId() == null) {
            return null;
        }
        BusinessSystemEntity entity = baseDao.selectById(request.getId());
        return ConvertUtils.sourceToTarget(entity, BusinessSystemDTO.class);
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
    public boolean check(BusinessSystemCheckRequest request) {
        if (request == null) {
            return false;
        }
        return existsByInstanceOrName(request.getInstance(), request.getName(), request.getId());
    }

    @Override
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
            BusinessSystemEntity entity = new BusinessSystemEntity();
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
        ExcelUtils.exportExcel(response, "业务系统导入模板", "业务系统导入模板", new ArrayList<>(), BusinessSystemImportExcel.class);
    }

    @Override
    public void export(BusinessSystemPageRequest request, HttpServletResponse response) throws Exception {
        LambdaQueryWrapper<BusinessSystemEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(StrUtil.isNotBlank(request.getInstance()), BusinessSystemEntity::getInstance, request.getInstance());
        wrapper.like(StrUtil.isNotBlank(request.getName()), BusinessSystemEntity::getName, request.getName());
        wrapper.eq(StrUtil.isNotBlank(request.getAreaName()), BusinessSystemEntity::getAreaName, request.getAreaName());
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
        if (StrUtil.isBlank(instance) && StrUtil.isBlank(name)) {
            return false;
        }
        LambdaQueryWrapper<BusinessSystemEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.and((query) -> {
            if (StrUtil.isNotBlank(instance)) {
                query.or().eq(BusinessSystemEntity::getInstance, instance);
            }
            if (StrUtil.isNotBlank(name)) {
                query.or().eq(BusinessSystemEntity::getName, name);
            }
        });
        if (excludeId != null) {
            wrapper.ne(BusinessSystemEntity::getId, excludeId);
        }
        return baseDao.selectCount(wrapper) > 0;
    }

    private Page<BusinessSystemEntity> buildPage(BusinessSystemPageRequest request) {
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
        Page<BusinessSystemEntity> page = new Page<>(curPage, limit);
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

    private void validateUnique(BusinessSystemDTO dto) {
        if (dto != null && existsByInstanceOrName(dto.getInstance(), dto.getName(), dto.getId())) {
            throw new RenException("地址或名称已存在");
        }
    }
}
