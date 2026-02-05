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
import net.leoch.common.redis.RedisKeys;
import net.leoch.common.redis.RedisUtils;
import net.leoch.common.service.impl.CrudServiceImpl;
import net.leoch.common.utils.ConvertUtils;
import net.leoch.common.utils.ExcelUtils;
import net.leoch.common.validator.ValidatorUtils;
import net.leoch.common.validator.group.AddGroup;
import net.leoch.common.validator.group.DefaultGroup;
import net.leoch.common.validator.group.UpdateGroup;
import net.leoch.modules.ops.dao.BackupAgentDao;
import net.leoch.modules.ops.dao.DeviceBackupDao;
import net.leoch.modules.ops.dto.*;
import net.leoch.modules.ops.entity.BackupAgentEntity;
import net.leoch.modules.ops.entity.DeviceBackupEntity;
import net.leoch.modules.ops.excel.BackupAgentExcel;
import net.leoch.modules.ops.excel.template.BackupAgentImportExcel;
import net.leoch.modules.ops.service.BackupAgentService;
import net.leoch.modules.security.user.SecurityUser;
import org.springframework.stereotype.Service;

import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URL;
import java.util.*;

/**
 * 备份节点表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Service
public class BackupAgentServiceImpl extends CrudServiceImpl<BackupAgentDao, BackupAgentEntity, BackupAgentDTO> implements BackupAgentService {

    private final DeviceBackupDao deviceBackupDao;
    private final RedisUtils redisUtils;

    public BackupAgentServiceImpl(DeviceBackupDao deviceBackupDao, RedisUtils redisUtils) {
        this.deviceBackupDao = deviceBackupDao;
        this.redisUtils = redisUtils;
    }

    @Override
    public QueryWrapper<BackupAgentEntity> getWrapper(Map<String, Object> params) {
        QueryWrapper<BackupAgentEntity> wrapper = new QueryWrapper<>();
        LambdaQueryWrapper<BackupAgentEntity> lambda = wrapper.lambda();
        String id = (String) params.get("id");
        String instance = (String) params.get("instance");
        String name = (String) params.get("name");
        String areaName = (String) params.get("areaName");
        String status = (String) params.get("status");
        lambda.eq(StrUtil.isNotBlank(id), BackupAgentEntity::getId, id);
        lambda.like(StrUtil.isNotBlank(instance), BackupAgentEntity::getInstance, instance);
        lambda.like(StrUtil.isNotBlank(name), BackupAgentEntity::getName, name);
        lambda.eq(StrUtil.isNotBlank(areaName), BackupAgentEntity::getAreaName, areaName);
        lambda.eq(StrUtil.isNotBlank(status), BackupAgentEntity::getStatus, status);
        return wrapper;
    }

    @Override
    public PageData<BackupAgentDTO> page(BackupAgentPageRequest request) {
        LambdaQueryWrapper<BackupAgentEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.like(StrUtil.isNotBlank(request.getInstance()), BackupAgentEntity::getInstance, request.getInstance());
        wrapper.like(StrUtil.isNotBlank(request.getName()), BackupAgentEntity::getName, request.getName());
        wrapper.eq(StrUtil.isNotBlank(request.getAreaName()), BackupAgentEntity::getAreaName, request.getAreaName());
        wrapper.eq(StrUtil.isNotBlank(request.getStatus()), BackupAgentEntity::getStatus, request.getStatus());
        Page<BackupAgentEntity> page = buildPage(request);
        IPage<BackupAgentEntity> result = baseDao.selectPage(page, wrapper);
        List<BackupAgentDTO> list = ConvertUtils.sourceToTarget(result.getRecords(), BackupAgentDTO.class);
        fillOnlineStatus(list);
        maskTokens(list);
        return new PageData<>(list, result.getTotal());
    }

    @Override
    public BackupAgentDTO get(BackupAgentIdRequest request) {
        if (request == null || request.getId() == null) {
            return null;
        }
        BackupAgentEntity entity = baseDao.selectById(request.getId());
        BackupAgentDTO dto = ConvertUtils.sourceToTarget(entity, BackupAgentDTO.class);
        if (dto != null) {
            fillOnlineStatus(Collections.singletonList(dto));
        }
        maskToken(dto);
        return dto;
    }

    @Override
    public void save(BackupAgentSaveRequest request) {
        ValidatorUtils.validateEntity(request, AddGroup.class, DefaultGroup.class);
        validateUnique(request);
        super.save(request);
    }

    @Override
    public void update(BackupAgentUpdateRequest request) {
        ValidatorUtils.validateEntity(request, UpdateGroup.class, DefaultGroup.class);
        validateUnique(request);
        if (request != null && request.getId() != null && StrUtil.isBlank(request.getToken())) {
            BackupAgentEntity existing = baseDao.selectById(request.getId());
            if (existing != null) {
                request.setToken(existing.getToken());
            }
        }
        super.update(request);
    }

    @Override
    public void updateStatus(BackupAgentStatusUpdateRequest request) {
        if (request == null) {
            return;
        }
        updateStatus(request.getIds(), request.getStatus());
    }

    @Override
    public boolean online(BackupAgentOnlineRequest request) {
        if (request == null || StrUtil.isBlank(request.getInstance())) {
            return false;
        }
        return checkAgentHealth(request.getInstance());
    }

    @Override
    public boolean check(BackupAgentCheckRequest request) {
        if (request == null) {
            return false;
        }
        return existsByInstanceOrName(request.getInstance(), request.getName(), request.getId());
    }

    @Override
    public OpsHostStatusSummaryDTO summary(BackupAgentPageRequest request) {
        LambdaQueryWrapper<BackupAgentEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.select(BackupAgentEntity::getInstance, BackupAgentEntity::getStatus);
        List<BackupAgentEntity> list = baseDao.selectList(wrapper);
        Map<String, Object> statusMap = redisUtils.hGetAll(RedisKeys.getBackupAgentOnlineKey());
        OpsHostStatusSummaryDTO summary = new OpsHostStatusSummaryDTO();
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
    public void importExcel(BackupAgentImportRequest request) throws Exception {
        if (request == null || request.getFile() == null || request.getFile().isEmpty()) {
            throw new RenException("上传文件不能为空");
        }
        List<BackupAgentImportExcel> dataList = EasyExcel.read(request.getFile().getInputStream()).head(BackupAgentImportExcel.class).sheet().doReadSync();
        if (CollUtil.isEmpty(dataList)) {
            throw new RenException("导入数据不能为空");
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
        insertBatch(entityList);
    }

    @Override
    public void template(HttpServletResponse response) throws Exception {
        ExcelUtils.exportExcel(response, "备份节点导入模板", "备份节点导入模板", new ArrayList<>(), BackupAgentImportExcel.class);
    }

    @Override
    public void export(BackupAgentPageRequest request, HttpServletResponse response) throws Exception {
        LambdaQueryWrapper<BackupAgentEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.like(StrUtil.isNotBlank(request.getInstance()), BackupAgentEntity::getInstance, request.getInstance());
        wrapper.like(StrUtil.isNotBlank(request.getName()), BackupAgentEntity::getName, request.getName());
        wrapper.eq(StrUtil.isNotBlank(request.getAreaName()), BackupAgentEntity::getAreaName, request.getAreaName());
        wrapper.eq(StrUtil.isNotBlank(request.getStatus()), BackupAgentEntity::getStatus, request.getStatus());
        List<BackupAgentEntity> list = baseDao.selectList(wrapper);
        List<BackupAgentDTO> dtoList = ConvertUtils.sourceToTarget(list, BackupAgentDTO.class);
        maskTokens(dtoList);
        ExcelUtils.exportExcelToTarget(response, null, "备份节点表", dtoList, BackupAgentExcel.class);
    }

    @Override
    public void delete(BackupAgentDeleteRequest request) {
        if (request == null || request.getIds() == null || request.getIds().length == 0) {
            return;
        }
        delete(request.getIds());
    }

    @Override
    public List<BackupAgentDTO> list(Map<String, Object> params) {
        List<BackupAgentDTO> list = super.list(params);
        maskTokens(list);
        return list;
    }

    @Override
    public BackupAgentDTO get(Long id) {
        BackupAgentDTO dto = super.get(id);
        maskToken(dto);
        return dto;
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
        return baseDao.selectCount(wrapper) > 0;
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
        baseDao.update(entity, wrapper);
    }

    @Override
    public void delete(Long[] ids) {
        if (ids == null || ids.length == 0) {
            return;
        }
        LambdaQueryWrapper<DeviceBackupEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.in(DeviceBackupEntity::getAgentId, Arrays.asList(ids));
        int used = Math.toIntExact(deviceBackupDao.selectCount(wrapper));
        if (used > 0) {
            throw new RenException("存在绑定的备份设备，无法删除");
        }
        super.delete(ids);
    }

    private Page<BackupAgentEntity> buildPage(BackupAgentPageRequest request) {
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
            if (Constant.ASC.equalsIgnoreCase(request.getOrder())) {
                page.addOrder(OrderItem.asc(request.getOrderField()));
            } else {
                page.addOrder(OrderItem.desc(request.getOrderField()));
            }
        }
        return page;
    }

    private void validateUnique(BackupAgentDTO dto) {
        if (dto != null && existsByInstanceOrName(dto.getInstance(), dto.getName(), dto.getId())) {
            throw new RenException("地址或名称已存在");
        }
    }

    private void maskTokens(List<BackupAgentDTO> list) {
        if (list == null || list.isEmpty()) {
            return;
        }
        for (BackupAgentDTO dto : list) {
            maskToken(dto);
        }
    }

    private void fillOnlineStatus(List<BackupAgentDTO> list) {
        if (list == null || list.isEmpty()) {
            return;
        }
        Map<String, Object> statusMap = redisUtils.hGetAll(RedisKeys.getBackupAgentOnlineKey());
        for (BackupAgentDTO dto : list) {
            String instance = dto.getInstance();
            dto.setOnlineStatus(OnlineStatusSupport.resolveOnlineStatus(statusMap == null ? null : statusMap.get(instance)));
        }
    }

    private void maskToken(BackupAgentDTO dto) {
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
        } catch (Exception ignore) {
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
                    } catch (Exception ignore) {
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
        } catch (Exception ignore) {
            if (trimmed.startsWith("http://") || trimmed.startsWith("https://")) {
                return trimmed.endsWith("/healthz") ? trimmed : trimmed + (trimmed.endsWith("/") ? "healthz" : "/healthz");
            }
            return "http://" + trimmed + ":8120/healthz";
        }
    }
}
