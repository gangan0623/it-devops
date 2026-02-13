package net.leoch.modules.ops.service.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.alibaba.excel.EasyExcel;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.metadata.OrderItem;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.base.Constant;
import net.leoch.common.data.page.PageData;
import net.leoch.common.data.validator.ValidatorUtils;
import net.leoch.common.data.validator.group.AddGroup;
import net.leoch.common.data.validator.group.DefaultGroup;
import net.leoch.common.data.validator.group.UpdateGroup;
import net.leoch.common.exception.ServiceException;
import net.leoch.common.integration.excel.DeviceBackupExcel;
import net.leoch.common.integration.excel.template.DeviceBackupImportExcel;
import net.leoch.common.integration.security.SecurityUser;
import net.leoch.common.utils.excel.ExcelUtils;
import net.leoch.common.utils.ops.PingUtils;
import net.leoch.common.utils.redis.RedisKeys;
import net.leoch.common.utils.redis.RedisUtils;
import net.leoch.framework.config.ops.OnlineStatusConfig;
import net.leoch.modules.ops.entity.BackupAgentEntity;
import net.leoch.modules.ops.entity.DeviceBackupEntity;
import net.leoch.modules.ops.mapper.BackupAgentMapper;
import net.leoch.modules.ops.mapper.DeviceBackupMapper;
import net.leoch.modules.ops.service.IDeviceBackupService;
import net.leoch.modules.ops.vo.req.*;
import net.leoch.modules.ops.vo.rsp.DeviceBackupRsp;
import net.leoch.modules.ops.vo.rsp.OpsHostStatusSummaryRsp;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 设备备份表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Slf4j
@Service
public class DeviceBackupServiceImpl extends ServiceImpl<DeviceBackupMapper, DeviceBackupEntity> implements IDeviceBackupService {

    /** 允许排序的数据库列名白名单 */
    private static final Set<String> ALLOWED_ORDER_FIELDS = Set.of(
            "id", "instance", "name", "area_name", "group_name", "device_model",
            "status", "agent_id", "create_date", "update_date");

    private final BackupAgentMapper backupAgentMapper;
    private final RedisUtils redisUtils;
    private final OnlineStatusConfig properties;

    public DeviceBackupServiceImpl(BackupAgentMapper backupAgentMapper, RedisUtils redisUtils,
                                   OnlineStatusConfig properties) {
        this.backupAgentMapper = backupAgentMapper;
        this.redisUtils = redisUtils;
        this.properties = properties;
    }

    @Override
    public PageData<DeviceBackupRsp> page(DeviceBackupPageReq request) {
        LambdaQueryWrapper<DeviceBackupEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.like(StrUtil.isNotBlank(request.getInstance()), DeviceBackupEntity::getInstance, request.getInstance());
        wrapper.like(StrUtil.isNotBlank(request.getName()), DeviceBackupEntity::getName, request.getName());
        wrapper.eq(StrUtil.isNotBlank(request.getAreaName()), DeviceBackupEntity::getAreaName, request.getAreaName());
        wrapper.eq(StrUtil.isNotBlank(request.getGroupName()), DeviceBackupEntity::getGroupName, request.getGroupName());
        wrapper.eq(StrUtil.isNotBlank(request.getDeviceModel()), DeviceBackupEntity::getDeviceModel, request.getDeviceModel());
        wrapper.eq(StrUtil.isNotBlank(request.getStatus()), DeviceBackupEntity::getStatus, request.getStatus());
        wrapper.eq(StrUtil.isNotBlank(request.getAgentId()), DeviceBackupEntity::getAgentId, request.getAgentId());
        Page<DeviceBackupEntity> page = buildPage(request);
        IPage<DeviceBackupEntity> result = this.page(page, wrapper);
        List<DeviceBackupRsp> list = BeanUtil.copyToList(result.getRecords(), DeviceBackupRsp.class);
        fillOnlineStatus(list);
        fillAgentNames(list);
        maskPasswords(list);
        return new PageData<>(list, result.getTotal());
    }

    @Override
    public DeviceBackupRsp get(DeviceBackupIdReq request) {
        if (request == null || request.getId() == null) {
            return null;
        }
        DeviceBackupEntity entity = this.getById(request.getId());
        DeviceBackupRsp dto = BeanUtil.copyProperties(entity, DeviceBackupRsp.class);
        if (dto != null) {
            fillOnlineStatus(Arrays.asList(dto));
            fillAgentNames(Arrays.asList(dto));
            maskPassword(dto);
        }
        return dto;
    }

    @Override
    public void save(DeviceBackupSaveReq request) {
        log.info("[DeviceBackup] 开始保存, instance={}", request != null ? request.getInstance() : null);
        ValidatorUtils.validateEntity(request, AddGroup.class, DefaultGroup.class);
        validateUnique(request.getId(), request.getInstance(), request.getName());
        DeviceBackupEntity entity = BeanUtil.copyProperties(request, DeviceBackupEntity.class);
        this.save(entity);
    }

    @Override
    public void update(DeviceBackupUpdateReq request) {
        log.info("[DeviceBackup] 开始更新, id={}", request != null ? request.getId() : null);
        ValidatorUtils.validateEntity(request, UpdateGroup.class, DefaultGroup.class);
        validateUnique(request.getId(), request.getInstance(), request.getName());
        if (request != null && request.getId() != null && StrUtil.isBlank(request.getPassword())) {
            DeviceBackupEntity existing = this.getById(request.getId());
            if (existing != null && StrUtil.isNotBlank(existing.getPassword())) {
                request.setPassword(existing.getPassword());
            }
        }
        DeviceBackupEntity entity = BeanUtil.copyProperties(request, DeviceBackupEntity.class);
        this.updateById(entity);
    }

    @Override
    public void updateStatus(DeviceBackupStatusUpdateReq request) {
        if (request == null) {
            return;
        }
        updateStatus(request.getIds(), request.getStatus());
    }

    @Override
    public boolean online(DeviceBackupOnlineReq request) {
        if (request == null || StrUtil.isBlank(request.getInstance())) {
            return false;
        }
        int timeout = properties.getOnlineStatus().getTimeout().getDevice();
        return PingUtils.isReachable(request.getInstance(), timeout);
    }

    @Override
    public boolean check(DeviceBackupCheckReq request) {
        if (request == null) {
            return false;
        }
        return existsByInstanceOrName(request.getInstance(), request.getName(), request.getId());
    }

    @Override
    public OpsHostStatusSummaryRsp summary(DeviceBackupPageReq request) {
        LambdaQueryWrapper<DeviceBackupEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.select(DeviceBackupEntity::getInstance, DeviceBackupEntity::getStatus);
        List<DeviceBackupEntity> list = this.list(wrapper);
        Map<String, Object> statusMap = redisUtils.hGetAll(RedisKeys.getDeviceBackupOnlineKey());
        OpsHostStatusSummaryRsp summary = new OpsHostStatusSummaryRsp();
        summary.setTotalCount((long) list.size());
        for (DeviceBackupEntity item : list) {
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
    @Transactional(rollbackFor = Exception.class)
    public void importExcel(DeviceBackupImportReq request) throws Exception {
        if (request == null || request.getFile() == null || request.getFile().isEmpty()) {
            throw new ServiceException("上传文件不能为空");
        }
        List<DeviceBackupImportExcel> dataList = EasyExcel.read(request.getFile().getInputStream()).head(DeviceBackupImportExcel.class).sheet().doReadSync();
        if (CollUtil.isEmpty(dataList)) {
            throw new ServiceException("导入数据不能为空");
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

        // 分批处理，避免大批量数据导致 SQL 超时或 OOM
        final int BATCH_SIZE = 1000;
        if (entityList.size() > BATCH_SIZE) {
            log.info("[设备备份] Excel 导入分批处理, 总数={}, 批次大小={}", entityList.size(), BATCH_SIZE);
            List<List<DeviceBackupEntity>> batches = CollUtil.split(entityList, BATCH_SIZE);
            for (int i = 0; i < batches.size(); i++) {
                log.debug("[设备备份] 处理第 {}/{} 批, 数量={}", i + 1, batches.size(), batches.get(i).size());
                this.saveBatch(batches.get(i));
            }
        } else {
            this.saveBatch(entityList);
        }
        log.info("[设备备份] Excel 导入完成, 总数={}", entityList.size());
    }

    @Override
    public void template(HttpServletResponse response) throws Exception {
        ExcelUtils.exportExcel(response, "设备备份导入模板", "设备备份导入模板", new ArrayList<>(), DeviceBackupImportExcel.class);
    }

    @Override
    public void export(DeviceBackupPageReq request, HttpServletResponse response) throws Exception {
        LambdaQueryWrapper<DeviceBackupEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.like(StrUtil.isNotBlank(request.getInstance()), DeviceBackupEntity::getInstance, request.getInstance());
        wrapper.like(StrUtil.isNotBlank(request.getName()), DeviceBackupEntity::getName, request.getName());
        wrapper.eq(StrUtil.isNotBlank(request.getAreaName()), DeviceBackupEntity::getAreaName, request.getAreaName());
        wrapper.eq(StrUtil.isNotBlank(request.getGroupName()), DeviceBackupEntity::getGroupName, request.getGroupName());
        wrapper.eq(StrUtil.isNotBlank(request.getDeviceModel()), DeviceBackupEntity::getDeviceModel, request.getDeviceModel());
        wrapper.eq(StrUtil.isNotBlank(request.getStatus()), DeviceBackupEntity::getStatus, request.getStatus());
        wrapper.eq(StrUtil.isNotBlank(request.getAgentId()), DeviceBackupEntity::getAgentId, request.getAgentId());
        List<DeviceBackupEntity> list = this.list(wrapper);
        List<DeviceBackupRsp> dtoList = BeanUtil.copyToList(list, DeviceBackupRsp.class);
        fillAgentNames(dtoList);
        maskPasswords(dtoList);
        ExcelUtils.exportExcelToTarget(response, null, "设备备份表", dtoList, DeviceBackupExcel.class);
    }

    @Override
    public void delete(DeviceBackupDeleteReq request) {
        if (request == null || request.getIds() == null || request.getIds().length == 0) {
            return;
        }
        this.removeByIds(Arrays.asList(request.getIds()));
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
        return this.count(wrapper) > 0;
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
        this.update(entity, wrapper);
    }

    private Page<DeviceBackupEntity> buildPage(DeviceBackupPageReq request) {
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
            String orderField = request.getOrderField();
            if (!ALLOWED_ORDER_FIELDS.contains(orderField)) {
                log.warn("[设备备份] 非法排序字段: {}", orderField);
            } else if (Constant.ASC.equalsIgnoreCase(request.getOrder())) {
                page.addOrder(OrderItem.asc(orderField));
            } else {
                page.addOrder(OrderItem.desc(orderField));
            }
        }
        return page;
    }

    private void validateUnique(Long id, String instance, String name) {
        if (existsByInstanceOrName(instance, name, id)) {
            throw new ServiceException("地址或名称已存在");
        }
    }

    private void fillAgentNames(List<DeviceBackupRsp> list) {
        if (list == null || list.isEmpty()) {
            return;
        }
        List<Long> agentIds = list.stream()
            .map(DeviceBackupRsp::getAgentId)
            .filter(Objects::nonNull)
            .distinct()
            .collect(Collectors.toList());
        if (agentIds.isEmpty()) {
            return;
        }
        LambdaQueryWrapper<BackupAgentEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.select(BackupAgentEntity::getId, BackupAgentEntity::getName);
        wrapper.in(BackupAgentEntity::getId, agentIds);
        List<BackupAgentEntity> agents = backupAgentMapper.selectList(wrapper);
        if (agents == null || agents.isEmpty()) {
            return;
        }
        Map<Long, String> agentNameMap = new HashMap<>((int) (agents.size() / 0.75) + 1);
        for (BackupAgentEntity agent : agents) {
            if (agent != null && agent.getId() != null) {
                agentNameMap.put(agent.getId(), agent.getName());
            }
        }
        for (DeviceBackupRsp dto : list) {
            if (dto != null && dto.getAgentId() != null) {
                dto.setAgentName(agentNameMap.get(dto.getAgentId()));
            }
        }
    }

    private void fillOnlineStatus(List<DeviceBackupRsp> list) {
        if (list == null || list.isEmpty()) {
            return;
        }
        Map<String, Object> statusMap = redisUtils.hGetAll(RedisKeys.getDeviceBackupOnlineKey());
        for (DeviceBackupRsp dto : list) {
            String instance = dto.getInstance();
            dto.setOnlineStatus(OnlineStatusSupport.resolveOnlineStatus(statusMap == null ? null : statusMap.get(instance)));
        }
    }

    private void maskPasswords(List<DeviceBackupRsp> list) {
        if (list == null || list.isEmpty()) {
            return;
        }
        for (DeviceBackupRsp dto : list) {
            maskPassword(dto);
        }
    }

    private void maskPassword(DeviceBackupRsp dto) {
        if (dto != null) {
            dto.setPassword(null);
        }
    }
}
