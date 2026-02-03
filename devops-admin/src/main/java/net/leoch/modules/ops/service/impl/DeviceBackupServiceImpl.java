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
import net.leoch.modules.ops.dao.BackupAgentDao;
import net.leoch.modules.ops.dao.DeviceBackupDao;
import net.leoch.modules.ops.dto.*;
import net.leoch.modules.ops.entity.BackupAgentEntity;
import net.leoch.modules.ops.entity.DeviceBackupEntity;
import net.leoch.modules.ops.excel.DeviceBackupExcel;
import net.leoch.modules.ops.excel.template.DeviceBackupImportExcel;
import net.leoch.modules.ops.service.DeviceBackupService;
import net.leoch.modules.security.user.SecurityUser;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 设备备份表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Service
public class DeviceBackupServiceImpl extends CrudServiceImpl<DeviceBackupDao, DeviceBackupEntity, DeviceBackupDTO> implements DeviceBackupService {

    private final BackupAgentDao backupAgentDao;

    public DeviceBackupServiceImpl(BackupAgentDao backupAgentDao) {
        this.backupAgentDao = backupAgentDao;
    }

    @Override
    public QueryWrapper<DeviceBackupEntity> getWrapper(Map<String, Object> params) {
        QueryWrapper<DeviceBackupEntity> wrapper = new QueryWrapper<>();
        LambdaQueryWrapper<DeviceBackupEntity> lambda = wrapper.lambda();
        String id = (String) params.get("id");
        String instance = (String) params.get("instance");
        String name = (String) params.get("name");
        String areaName = (String) params.get("areaName");
        String groupName = (String) params.get("groupName");
        String deviceModel = (String) params.get("deviceModel");
        String status = (String) params.get("status");
        String agentId = (String) params.get("agentId");
        lambda.eq(StrUtil.isNotBlank(id), DeviceBackupEntity::getId, id);
        lambda.like(StrUtil.isNotBlank(instance), DeviceBackupEntity::getInstance, instance);
        lambda.like(StrUtil.isNotBlank(name), DeviceBackupEntity::getName, name);
        lambda.eq(StrUtil.isNotBlank(areaName), DeviceBackupEntity::getAreaName, areaName);
        lambda.eq(StrUtil.isNotBlank(groupName), DeviceBackupEntity::getGroupName, groupName);
        lambda.eq(StrUtil.isNotBlank(deviceModel), DeviceBackupEntity::getDeviceModel, deviceModel);
        lambda.eq(StrUtil.isNotBlank(status), DeviceBackupEntity::getStatus, status);
        lambda.eq(StrUtil.isNotBlank(agentId), DeviceBackupEntity::getAgentId, agentId);
        return wrapper;
    }

    @Override
    public PageData<DeviceBackupDTO> page(DeviceBackupPageRequest request) {
        LambdaQueryWrapper<DeviceBackupEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.like(StrUtil.isNotBlank(request.getInstance()), DeviceBackupEntity::getInstance, request.getInstance());
        wrapper.like(StrUtil.isNotBlank(request.getName()), DeviceBackupEntity::getName, request.getName());
        wrapper.eq(StrUtil.isNotBlank(request.getAreaName()), DeviceBackupEntity::getAreaName, request.getAreaName());
        wrapper.eq(StrUtil.isNotBlank(request.getGroupName()), DeviceBackupEntity::getGroupName, request.getGroupName());
        wrapper.eq(StrUtil.isNotBlank(request.getDeviceModel()), DeviceBackupEntity::getDeviceModel, request.getDeviceModel());
        wrapper.eq(StrUtil.isNotBlank(request.getStatus()), DeviceBackupEntity::getStatus, request.getStatus());
        wrapper.eq(StrUtil.isNotBlank(request.getAgentId()), DeviceBackupEntity::getAgentId, request.getAgentId());
        Page<DeviceBackupEntity> page = buildPage(request);
        IPage<DeviceBackupEntity> result = baseDao.selectPage(page, wrapper);
        List<DeviceBackupDTO> list = ConvertUtils.sourceToTarget(result.getRecords(), DeviceBackupDTO.class);
        fillAgentNames(list);
        maskPasswords(list);
        return new PageData<>(list, result.getTotal());
    }

    @Override
    public DeviceBackupDTO get(DeviceBackupIdRequest request) {
        if (request == null || request.getId() == null) {
            return null;
        }
        DeviceBackupEntity entity = baseDao.selectById(request.getId());
        DeviceBackupDTO dto = ConvertUtils.sourceToTarget(entity, DeviceBackupDTO.class);
        if (dto != null) {
            fillAgentNames(Arrays.asList(dto));
            maskPassword(dto);
        }
        return dto;
    }

    @Override
    public void save(DeviceBackupSaveRequest request) {
        ValidatorUtils.validateEntity(request, AddGroup.class, DefaultGroup.class);
        validateUnique(request);
        super.save(request);
    }

    @Override
    public void update(DeviceBackupUpdateRequest request) {
        ValidatorUtils.validateEntity(request, UpdateGroup.class, DefaultGroup.class);
        validateUnique(request);
        if (request != null && request.getId() != null && StrUtil.isBlank(request.getPassword())) {
            DeviceBackupEntity existing = baseDao.selectById(request.getId());
            if (existing != null) {
                request.setPassword(existing.getPassword());
            }
        }
        super.update(request);
    }

    @Override
    public void updateStatus(DeviceBackupStatusUpdateRequest request) {
        if (request == null) {
            return;
        }
        updateStatus(request.getIds(), request.getStatus());
    }

    @Override
    public boolean online(DeviceBackupOnlineRequest request) {
        if (request == null || StrUtil.isBlank(request.getInstance())) {
            return false;
        }
        return PingUtils.isReachable(request.getInstance(), 2000);
    }

    @Override
    public boolean check(DeviceBackupCheckRequest request) {
        if (request == null) {
            return false;
        }
        return existsByInstanceOrName(request.getInstance(), request.getName(), request.getId());
    }

    @Override
    public void importExcel(DeviceBackupImportRequest request) throws Exception {
        if (request == null || request.getFile() == null || request.getFile().isEmpty()) {
            throw new RenException("上传文件不能为空");
        }
        List<DeviceBackupImportExcel> dataList = EasyExcel.read(request.getFile().getInputStream()).head(DeviceBackupImportExcel.class).sheet().doReadSync();
        if (CollUtil.isEmpty(dataList)) {
            throw new RenException("导入数据不能为空");
        }
        List<DeviceBackupEntity> entityList = new ArrayList<>(dataList.size());
        for (DeviceBackupImportExcel item : dataList) {
            DeviceBackupEntity entity = new DeviceBackupEntity();
            entity.setInstance(item.getInstance());
            entity.setName(item.getName());
            entity.setUsername(item.getUsername());
            entity.setPassword(item.getPassword());
            entity.setAreaName(item.getAreaName());
            entity.setGroupName(item.getGroupName());
            entity.setDeviceModel(item.getDeviceModel());
            entity.setStatus(item.getStatus());
            entity.setAgentId(item.getAgentId());
            entityList.add(entity);
        }
        insertBatch(entityList);
    }

    @Override
    public void template(HttpServletResponse response) throws Exception {
        ExcelUtils.exportExcel(response, "设备备份导入模板", "设备备份导入模板", new ArrayList<>(), DeviceBackupImportExcel.class);
    }

    @Override
    public void export(DeviceBackupPageRequest request, HttpServletResponse response) throws Exception {
        LambdaQueryWrapper<DeviceBackupEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.like(StrUtil.isNotBlank(request.getInstance()), DeviceBackupEntity::getInstance, request.getInstance());
        wrapper.like(StrUtil.isNotBlank(request.getName()), DeviceBackupEntity::getName, request.getName());
        wrapper.eq(StrUtil.isNotBlank(request.getAreaName()), DeviceBackupEntity::getAreaName, request.getAreaName());
        wrapper.eq(StrUtil.isNotBlank(request.getGroupName()), DeviceBackupEntity::getGroupName, request.getGroupName());
        wrapper.eq(StrUtil.isNotBlank(request.getDeviceModel()), DeviceBackupEntity::getDeviceModel, request.getDeviceModel());
        wrapper.eq(StrUtil.isNotBlank(request.getStatus()), DeviceBackupEntity::getStatus, request.getStatus());
        wrapper.eq(StrUtil.isNotBlank(request.getAgentId()), DeviceBackupEntity::getAgentId, request.getAgentId());
        List<DeviceBackupEntity> list = baseDao.selectList(wrapper);
        List<DeviceBackupDTO> dtoList = ConvertUtils.sourceToTarget(list, DeviceBackupDTO.class);
        fillAgentNames(dtoList);
        maskPasswords(dtoList);
        ExcelUtils.exportExcelToTarget(response, null, "设备备份表", dtoList, DeviceBackupExcel.class);
    }

    @Override
    public void delete(DeviceBackupDeleteRequest request) {
        if (request == null || request.getIds() == null || request.getIds().length == 0) {
            return;
        }
        super.delete(request.getIds());
    }

    @Override
    public List<DeviceBackupDTO> list(Map<String, Object> params) {
        List<DeviceBackupDTO> list = super.list(params);
        fillAgentNames(list);
        maskPasswords(list);
        return list;
    }

    @Override
    public DeviceBackupDTO get(Long id) {
        DeviceBackupDTO dto = super.get(id);
        if (dto == null) {
            return null;
        }
        fillAgentNames(Arrays.asList(dto));
        maskPassword(dto);
        return dto;
    }

    @Override
    public boolean existsByInstanceOrName(String instance, String name, Long excludeId) {
        if (StrUtil.isBlank(instance) && StrUtil.isBlank(name)) {
            return false;
        }
        LambdaQueryWrapper<DeviceBackupEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.and((query) -> {
            if (StrUtil.isNotBlank(instance)) {
                query.or().eq(DeviceBackupEntity::getInstance, instance);
            }
            if (StrUtil.isNotBlank(name)) {
                query.or().eq(DeviceBackupEntity::getName, name);
            }
        });
        if (excludeId != null) {
            wrapper.ne(DeviceBackupEntity::getId, excludeId);
        }
        return baseDao.selectCount(wrapper) > 0;
    }

    @Override
    public void updateStatus(Long[] ids, Integer status) {
        if (ids == null || ids.length == 0) {
            return;
        }
        DeviceBackupEntity entity = new DeviceBackupEntity();
        entity.setStatus(status);
        entity.setUpdater(SecurityUser.getUserId());
        entity.setUpdateDate(new Date());
        LambdaUpdateWrapper<DeviceBackupEntity> wrapper = new LambdaUpdateWrapper<>();
        wrapper.in(DeviceBackupEntity::getId, Arrays.asList(ids));
        baseDao.update(entity, wrapper);
    }

    private Page<DeviceBackupEntity> buildPage(DeviceBackupPageRequest request) {
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
        Page<DeviceBackupEntity> page = new Page<>(curPage, limit);
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

    private void validateUnique(DeviceBackupDTO dto) {
        if (dto != null && existsByInstanceOrName(dto.getInstance(), dto.getName(), dto.getId())) {
            throw new RenException("地址或名称已存在");
        }
    }

    private void fillAgentNames(List<DeviceBackupDTO> list) {
        if (list == null || list.isEmpty()) {
            return;
        }
        List<Long> agentIds = list.stream()
            .map(DeviceBackupDTO::getAgentId)
            .filter(Objects::nonNull)
            .distinct()
            .collect(Collectors.toList());
        if (agentIds.isEmpty()) {
            return;
        }
        LambdaQueryWrapper<BackupAgentEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.select(BackupAgentEntity::getId, BackupAgentEntity::getName);
        wrapper.in(BackupAgentEntity::getId, agentIds);
        List<BackupAgentEntity> agents = backupAgentDao.selectList(wrapper);
        Map<Long, String> agentNameMap = new HashMap<>();
        for (BackupAgentEntity agent : agents) {
            agentNameMap.put(agent.getId(), agent.getName());
        }
        for (DeviceBackupDTO dto : list) {
            if (dto.getAgentId() != null) {
                dto.setAgentName(agentNameMap.get(dto.getAgentId()));
            }
        }
    }

    private void maskPasswords(List<DeviceBackupDTO> list) {
        if (list == null || list.isEmpty()) {
            return;
        }
        for (DeviceBackupDTO dto : list) {
            maskPassword(dto);
        }
    }

    private void maskPassword(DeviceBackupDTO dto) {
        if (dto != null) {
            dto.setPassword(null);
        }
    }
}
