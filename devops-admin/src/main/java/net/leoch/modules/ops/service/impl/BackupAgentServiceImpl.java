package net.leoch.modules.ops.service.impl;

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
import net.leoch.common.base.Constant;
import net.leoch.common.exception.ServiceException;
import net.leoch.common.data.page.PageData;
import net.leoch.common.utils.redis.RedisKeys;
import net.leoch.common.utils.redis.RedisUtils;
import cn.hutool.core.bean.BeanUtil;
import net.leoch.common.utils.excel.ExcelUtils;
import net.leoch.common.data.validator.ValidatorUtils;
import net.leoch.common.data.validator.group.AddGroup;
import net.leoch.common.data.validator.group.DefaultGroup;
import net.leoch.common.data.validator.group.UpdateGroup;
import net.leoch.modules.ops.mapper.BackupAgentMapper;
import net.leoch.modules.ops.mapper.DeviceBackupMapper;
import net.leoch.modules.ops.vo.req.*;
import net.leoch.modules.ops.vo.rsp.*;
import net.leoch.modules.ops.entity.BackupAgentEntity;
import net.leoch.modules.ops.entity.DeviceBackupEntity;
import net.leoch.common.integration.excel.BackupAgentExcel;
import net.leoch.common.integration.excel.template.BackupAgentImportExcel;
import net.leoch.modules.ops.service.IBackupAgentService;
import net.leoch.common.integration.security.SecurityUser;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URL;
import java.util.*;
import java.util.Set;

/**
 * 备份节点表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Slf4j
@Service
public class BackupAgentServiceImpl extends ServiceImpl<BackupAgentMapper, BackupAgentEntity> implements IBackupAgentService {

    /** 允许排序的数据库列名白名单 */
    private static final Set<String> ALLOWED_ORDER_FIELDS = Set.of(
            "id", "instance", "name", "area_name", "status", "create_date", "update_date");

    private final DeviceBackupMapper deviceBackupMapper;
    private final RedisUtils redisUtils;

    public BackupAgentServiceImpl(DeviceBackupMapper deviceBackupMapper, RedisUtils redisUtils) {
        this.deviceBackupMapper = deviceBackupMapper;
        this.redisUtils = redisUtils;
    }

    @Override
    public PageData<BackupAgentRsp> page(BackupAgentPageReq request) {
        Page<BackupAgentEntity> page = buildPage(request);
        IPage<BackupAgentEntity> result = this.page(page,
                new LambdaQueryWrapper<BackupAgentEntity>()
                        .like(StrUtil.isNotBlank(request.getInstance()), BackupAgentEntity::getInstance, request.getInstance())
                        .like(StrUtil.isNotBlank(request.getName()), BackupAgentEntity::getName, request.getName())
                        .eq(StrUtil.isNotBlank(request.getAreaName()), BackupAgentEntity::getAreaName, request.getAreaName())
                        .eq(StrUtil.isNotBlank(request.getStatus()), BackupAgentEntity::getStatus, request.getStatus())
        );
        List<BackupAgentRsp> list = BeanUtil.copyToList(result.getRecords(), BackupAgentRsp.class);
        fillOnlineStatus(list);
        maskTokens(list);
        return new PageData<>(list, result.getTotal());
    }

    @Override
    public BackupAgentRsp get(BackupAgentIdReq request) {
        if (request == null || request.getId() == null) {
            return null;
        }
        BackupAgentEntity entity = this.getById(request.getId());
        BackupAgentRsp dto = BeanUtil.copyProperties(entity, BackupAgentRsp.class);
        if (dto != null) {
            fillOnlineStatus(Collections.singletonList(dto));
        }
        maskToken(dto);
        return dto;
    }

    @Override
    public void save(BackupAgentSaveReq request) {
        ValidatorUtils.validateEntity(request, AddGroup.class, DefaultGroup.class);
        validateUnique(request.getId(), request.getInstance(), request.getName());
        BackupAgentEntity entity = BeanUtil.copyProperties(request, BackupAgentEntity.class);
        this.save(entity);
    }

    @Override
    public void update(BackupAgentUpdateReq request) {
        ValidatorUtils.validateEntity(request, UpdateGroup.class, DefaultGroup.class);
        validateUnique(request.getId(), request.getInstance(), request.getName());
        if (request != null && request.getId() != null && StrUtil.isBlank(request.getToken())) {
            BackupAgentEntity existing = this.getById(request.getId());
            if (existing != null) {
                request.setToken(existing.getToken());
            }
        }
        BackupAgentEntity entity = BeanUtil.copyProperties(request, BackupAgentEntity.class);
        this.updateById(entity);
    }

    @Override
    public void updateStatus(BackupAgentStatusUpdateReq request) {
        if (request == null) {
            return;
        }
        updateStatus(request.getIds(), request.getStatus());
    }

    @Override
    public boolean online(BackupAgentOnlineReq request) {
        if (request == null || StrUtil.isBlank(request.getInstance())) {
            return false;
        }
        return checkAgentHealth(request.getInstance());
    }

    @Override
    public boolean check(BackupAgentCheckReq request) {
        if (request == null) {
            return false;
        }
        return existsByInstanceOrName(request.getInstance(), request.getName(), request.getId());
    }

    @Override
    public OpsHostStatusSummaryRsp summary(BackupAgentPageReq request) {
        LambdaQueryWrapper<BackupAgentEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.select(BackupAgentEntity::getInstance, BackupAgentEntity::getStatus);
        List<BackupAgentEntity> list = this.list(wrapper);
        Map<String, Object> statusMap = redisUtils.hGetAll(RedisKeys.getBackupAgentOnlineKey());
        OpsHostStatusSummaryRsp summary = new OpsHostStatusSummaryRsp();
        summary.setTotalCount((long) list.size());
        for (BackupAgentEntity item : list) {
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
    public void importExcel(BackupAgentImportReq request) throws Exception {
        if (request == null || request.getFile() == null || request.getFile().isEmpty()) {
            throw new ServiceException("上传文件不能为空");
        }
        List<BackupAgentImportExcel> dataList = EasyExcel.read(request.getFile().getInputStream()).head(BackupAgentImportExcel.class).sheet().doReadSync();
        if (CollUtil.isEmpty(dataList)) {
            throw new ServiceException("导入数据不能为空");
        }
        List<BackupAgentEntity> entityList = new ArrayList<>(dataList.size());
        for (BackupAgentImportExcel item : dataList) {
            BackupAgentEntity entity = new BackupAgentEntity();
            entity.setInstance(item.getInstance());
            entity.setName(item.getName());
            entity.setAreaName(item.getAreaName());
            entity.setToken(item.getToken());
            entity.setStatus(item.getStatus());
            entityList.add(entity);
        }
        this.saveBatch(entityList);
    }

    @Override
    public void template(HttpServletResponse response) throws Exception {
        ExcelUtils.exportExcel(response, "备份节点导入模板", "备份节点导入模板", new ArrayList<>(), BackupAgentImportExcel.class);
    }

    @Override
    public void export(BackupAgentPageReq request, HttpServletResponse response) throws Exception {
        LambdaQueryWrapper<BackupAgentEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.like(StrUtil.isNotBlank(request.getInstance()), BackupAgentEntity::getInstance, request.getInstance());
        wrapper.like(StrUtil.isNotBlank(request.getName()), BackupAgentEntity::getName, request.getName());
        wrapper.eq(StrUtil.isNotBlank(request.getAreaName()), BackupAgentEntity::getAreaName, request.getAreaName());
        wrapper.eq(StrUtil.isNotBlank(request.getStatus()), BackupAgentEntity::getStatus, request.getStatus());
        List<BackupAgentEntity> list = this.list(wrapper);
        List<BackupAgentRsp> dtoList = BeanUtil.copyToList(list, BackupAgentRsp.class);
        maskTokens(dtoList);
        ExcelUtils.exportExcelToTarget(response, null, "备份节点表", dtoList, BackupAgentExcel.class);
    }

    @Override
    public void delete(BackupAgentDeleteReq request) {
        if (request == null || request.getIds() == null || request.getIds().length == 0) {
            return;
        }
        Long[] ids = request.getIds();
        LambdaQueryWrapper<DeviceBackupEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.in(DeviceBackupEntity::getAgentId, Arrays.asList(ids));
        int used = Math.toIntExact(deviceBackupMapper.selectCount(wrapper));
        if (used > 0) {
            throw new ServiceException("存在绑定的备份设备，无法删除");
        }
        this.removeByIds(Arrays.asList(ids));
    }

    @Override
    public boolean existsByInstanceOrName(String instance, String name, Long excludeId) {
        if (StrUtil.isBlank(instance) && StrUtil.isBlank(name)) {
            return false;
        }
        LambdaQueryWrapper<BackupAgentEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.and((query) -> {
            if (StrUtil.isNotBlank(instance)) {
                query.or().eq(BackupAgentEntity::getInstance, instance);
            }
            if (StrUtil.isNotBlank(name)) {
                query.or().eq(BackupAgentEntity::getName, name);
            }
        });
        if (excludeId != null) {
            wrapper.ne(BackupAgentEntity::getId, excludeId);
        }
        return this.count(wrapper) > 0;
    }

    @Override
    public void updateStatus(Long[] ids, Integer status) {
        if (ids == null || ids.length == 0) {
            return;
        }
        BackupAgentEntity entity = new BackupAgentEntity();
        entity.setStatus(status);
        entity.setUpdater(SecurityUser.getUserId());
        entity.setUpdateDate(new Date());
        LambdaUpdateWrapper<BackupAgentEntity> wrapper = new LambdaUpdateWrapper<>();
        wrapper.in(BackupAgentEntity::getId, Arrays.asList(ids));
        this.update(entity, wrapper);
    }

    private Page<BackupAgentEntity> buildPage(BackupAgentPageReq request) {
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
        Page<BackupAgentEntity> page = new Page<>(curPage, limit);
        if (request == null) {
            return page;
        }
        if (StrUtil.isNotBlank(request.getOrderField()) && StrUtil.isNotBlank(request.getOrder())) {
            String orderField = request.getOrderField();
            if (!ALLOWED_ORDER_FIELDS.contains(orderField)) {
                log.warn("[备份节点] 非法排序字段: {}", orderField);
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

    private void maskTokens(List<BackupAgentRsp> list) {
        if (list == null || list.isEmpty()) {
            return;
        }
        for (BackupAgentRsp dto : list) {
            maskToken(dto);
        }
    }

    private void fillOnlineStatus(List<BackupAgentRsp> list) {
        if (list == null || list.isEmpty()) {
            return;
        }
        Map<String, Object> statusMap = redisUtils.hGetAll(RedisKeys.getBackupAgentOnlineKey());
        for (BackupAgentRsp dto : list) {
            String instance = dto.getInstance();
            dto.setOnlineStatus(OnlineStatusSupport.resolveOnlineStatus(statusMap == null ? null : statusMap.get(instance)));
        }
    }

    private void maskToken(BackupAgentRsp dto) {
        if (dto != null) {
            dto.setToken(null);
        }
    }

    private boolean checkAgentHealth(String instance) {
        if (StrUtil.isBlank(instance)) {
            return false;
        }
        String url = buildHealthUrl(instance);
        HttpURLConnection connection = null;
        try {
            URL target = new URL(url);
            connection = (HttpURLConnection) target.openConnection();
            connection.setRequestMethod("GET");
            connection.setConnectTimeout(2000);
            connection.setReadTimeout(2000);
            connection.connect();
            int code = connection.getResponseCode();
            if (code != 200) {
                return false;
            }
            try (InputStream in = connection.getInputStream()) {
                byte[] bytes = in.readAllBytes();
                String body = new String(bytes);
                return body.contains("\"status\":\"ok\"") || body.contains("\"status\" : \"ok\"") || body.contains("\"status\": \"ok\"");
            }
        } catch (Exception e) {
            log.debug("[备份节点] 健康检查失败, instance: {}", instance, e);
            return false;
        } finally {
            if (connection != null) {
                connection.disconnect();
            }
        }
    }

    private String buildHealthUrl(String instance) {
        String trimmed = instance.trim();
        try {
            String candidate = trimmed;
            if (!candidate.startsWith("http://") && !candidate.startsWith("https://")) {
                candidate = "http://" + candidate;
            }
            URI uri = new URI(candidate);
            String scheme = uri.getScheme() == null ? "http" : uri.getScheme();
            String host = uri.getHost();
            int port = uri.getPort();
            String path = uri.getPath();
            if (host == null && candidate.startsWith("http://")) {
                String raw = candidate.substring("http://".length());
                int slash = raw.indexOf('/');
                String hostPort = slash > -1 ? raw.substring(0, slash) : raw;
                String[] parts = hostPort.split(":");
                host = parts[0];
                if (parts.length > 1) {
                    try {
                        port = Integer.parseInt(parts[1]);
                    } catch (Exception e) {
                        log.debug("[备份节点] 端口解析失败, value: {}", parts[1], e);
                        port = -1;
                    }
                }
                path = slash > -1 ? raw.substring(slash) : "";
            }
            if (port == -1) {
                port = 8120;
            }
            if (path == null || path.isEmpty() || "/".equals(path)) {
                path = "/healthz";
            } else if (!path.endsWith("/healthz")) {
                if (path.endsWith("/")) {
                    path = path + "healthz";
                } else {
                    path = path + "/healthz";
                }
            }
            return new URI(scheme, null, host, port, path, null, null).toString();
        } catch (Exception e) {
            log.debug("[备份节点] 健康检查URL构建失败, instance: {}", trimmed, e);
            if (trimmed.startsWith("http://") || trimmed.startsWith("https://")) {
                return trimmed.endsWith("/healthz") ? trimmed : trimmed + (trimmed.endsWith("/") ? "healthz" : "/healthz");
            }
            return "http://" + trimmed + ":8120/healthz";
        }
    }
}
