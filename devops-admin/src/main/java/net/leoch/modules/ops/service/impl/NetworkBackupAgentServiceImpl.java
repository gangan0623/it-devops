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
import net.leoch.common.integration.excel.NetworkBackupAgentExcel;
import net.leoch.common.integration.excel.template.NetworkBackupAgentImportExcel;
import net.leoch.common.integration.security.SecurityUser;
import net.leoch.common.utils.excel.ExcelUtils;
import net.leoch.framework.config.ops.OnlineStatusConfig;
import net.leoch.modules.ops.entity.NetworkBackupAgentEntity;
import net.leoch.modules.ops.entity.NetworkBackupDeviceEntity;
import net.leoch.modules.ops.mapper.NetworkBackupAgentMapper;
import net.leoch.modules.ops.mapper.NetworkBackupDeviceMapper;
import net.leoch.modules.ops.service.INetworkBackupAgentService;
import net.leoch.modules.ops.vo.req.*;
import net.leoch.modules.ops.vo.rsp.NetworkBackupAgentRsp;
import net.leoch.modules.ops.vo.rsp.OpsHostStatusSummaryRsp;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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
@Slf4j
@Service
public class NetworkBackupAgentServiceImpl extends ServiceImpl<NetworkBackupAgentMapper, NetworkBackupAgentEntity> implements INetworkBackupAgentService {

    /** 允许排序的数据库列名白名单 */
    private static final Set<String> ALLOWED_ORDER_FIELDS = Set.of(
            "id", "instance", "name", "area_name", "status", "create_date", "update_date");

    /** 健康检查响应模式 */
    private static final String[] HEALTH_OK_PATTERNS = {
            "\"status\":\"ok\"",
            "\"status\" : \"ok\"",
            "\"status\": \"ok\""
    };

    private final NetworkBackupDeviceMapper networkBackupDeviceMapper;
    private final OnlineStatusConfig properties;

    public NetworkBackupAgentServiceImpl(NetworkBackupDeviceMapper networkBackupDeviceMapper,
                                  OnlineStatusConfig properties) {
        this.networkBackupDeviceMapper = networkBackupDeviceMapper;
        this.properties = properties;
    }

    @Override
    public PageData<NetworkBackupAgentRsp> page(NetworkBackupAgentPageReq request) {
        Page<NetworkBackupAgentEntity> page = buildPage(request);
        IPage<NetworkBackupAgentEntity> result = this.page(page,
                new LambdaQueryWrapper<NetworkBackupAgentEntity>()
                        .like(StrUtil.isNotBlank(request.getInstance()), NetworkBackupAgentEntity::getInstance, request.getInstance())
                        .like(StrUtil.isNotBlank(request.getName()), NetworkBackupAgentEntity::getName, request.getName())
                        .eq(StrUtil.isNotBlank(request.getAreaName()), NetworkBackupAgentEntity::getAreaName, request.getAreaName())
                        .eq(StrUtil.isNotBlank(request.getStatus()), NetworkBackupAgentEntity::getStatus, request.getStatus())
                        .eq(StrUtil.isNotBlank(request.getOnlineStatus()), NetworkBackupAgentEntity::getOnlineStatus, request.getOnlineStatus())
        );
        List<NetworkBackupAgentRsp> list = BeanUtil.copyToList(result.getRecords(), NetworkBackupAgentRsp.class);
        fillOnlineStatus(list);
        maskTokens(list);
        return new PageData<>(list, result.getTotal());
    }

    @Override
    public NetworkBackupAgentRsp get(NetworkBackupAgentIdReq request) {
        if (request == null || request.getId() == null) {
            return null;
        }
        NetworkBackupAgentEntity entity = this.getById(request.getId());
        NetworkBackupAgentRsp dto = BeanUtil.copyProperties(entity, NetworkBackupAgentRsp.class);
        if (dto != null) {
            fillOnlineStatus(Collections.singletonList(dto));
        }
        maskToken(dto);
        return dto;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void save(NetworkBackupAgentSaveReq request) {
        log.info("[NetworkBackupAgent] 开始保存, request={}", request);
        ValidatorUtils.validateEntity(request, AddGroup.class, DefaultGroup.class);
        validateUnique(request.getId(), request.getInstance(), request.getName());
        NetworkBackupAgentEntity entity = BeanUtil.copyProperties(request, NetworkBackupAgentEntity.class);
        this.save(entity);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void update(NetworkBackupAgentUpdateReq request) {
        log.info("[NetworkBackupAgent] 开始更新, request={}", request);
        ValidatorUtils.validateEntity(request, UpdateGroup.class, DefaultGroup.class);
        validateUnique(request.getId(), request.getInstance(), request.getName());
        if (request != null && request.getId() != null && StrUtil.isBlank(request.getToken())) {
            NetworkBackupAgentEntity existing = this.getById(request.getId());
            if (existing != null) {
                request.setToken(existing.getToken());
            }
        }
        NetworkBackupAgentEntity entity = BeanUtil.copyProperties(request, NetworkBackupAgentEntity.class);
        this.updateById(entity);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateStatus(NetworkBackupAgentStatusUpdateReq request) {
        if (request == null) {
            return;
        }
        updateStatus(request.getIds(), request.getStatus());
    }

    @Override
    public boolean online(NetworkBackupAgentOnlineReq request) {
        if (request == null || StrUtil.isBlank(request.getInstance())) {
            return false;
        }
        return checkAgentHealth(request.getInstance());
    }

    @Override
    public boolean check(NetworkBackupAgentCheckReq request) {
        if (request == null) {
            return false;
        }
        return existsByInstanceOrName(request.getInstance(), request.getName(), request.getId());
    }

    @Override
    public OpsHostStatusSummaryRsp summary(NetworkBackupAgentPageReq request) {
        LambdaQueryWrapper<NetworkBackupAgentEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.select(NetworkBackupAgentEntity::getInstance, NetworkBackupAgentEntity::getStatus, NetworkBackupAgentEntity::getOnlineStatus);
        List<NetworkBackupAgentEntity> list = this.list(wrapper);
        OpsHostStatusSummaryRsp summary = new OpsHostStatusSummaryRsp();
        summary.setTotalCount((long) list.size());
        for (NetworkBackupAgentEntity item : list) {
            if (item == null) {
                continue;
            }
            Integer status = item.getStatus();
            if (Integer.valueOf(1).equals(status)) {
                summary.setEnabledCount(summary.getEnabledCount() + 1);
            } else if (Integer.valueOf(0).equals(status)) {
                summary.setDisabledCount(summary.getDisabledCount() + 1);
            }
            Boolean onlineStatus = item.getOnlineStatus();
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
    public void importExcel(NetworkBackupAgentImportReq request) throws Exception {
        if (request == null || request.getFile() == null || request.getFile().isEmpty()) {
            throw new ServiceException("上传文件不能为空");
        }
        List<NetworkBackupAgentImportExcel> dataList = EasyExcel.read(request.getFile().getInputStream()).head(NetworkBackupAgentImportExcel.class).sheet().doReadSync();
        if (CollUtil.isEmpty(dataList)) {
            throw new ServiceException("导入数据不能为空");
        }
        List<NetworkBackupAgentEntity> entityList = new ArrayList<>(dataList.size());
        for (NetworkBackupAgentImportExcel item : dataList) {
            NetworkBackupAgentEntity entity = new NetworkBackupAgentEntity();
            entity.setInstance(item.getInstance());
            entity.setName(item.getName());
            entity.setAreaName(item.getAreaName());
            entity.setToken(item.getToken());
            entity.setStatus(item.getStatus());
            entityList.add(entity);
        }

        // 分批处理，避免大批量数据导致 SQL 超时或 OOM
        final int BATCH_SIZE = 1000;
        if (entityList.size() > BATCH_SIZE) {
            log.info("[备份节点] Excel 导入分批处理, 总数={}, 批次大小={}", entityList.size(), BATCH_SIZE);
            List<List<NetworkBackupAgentEntity>> batches = CollUtil.split(entityList, BATCH_SIZE);
            for (int i = 0; i < batches.size(); i++) {
                log.debug("[备份节点] 处理第 {}/{} 批, 数量={}", i + 1, batches.size(), batches.get(i).size());
                this.saveBatch(batches.get(i));
            }
        } else {
            this.saveBatch(entityList);
        }
        log.info("[备份节点] Excel 导入完成, 总数={}", entityList.size());
    }

    @Override
    public void template(HttpServletResponse response) throws Exception {
        ExcelUtils.exportExcel(response, "备份节点导入模板", "备份节点导入模板", new ArrayList<>(), NetworkBackupAgentImportExcel.class);
    }

    @Override
    public void export(NetworkBackupAgentPageReq request, HttpServletResponse response) throws Exception {
        LambdaQueryWrapper<NetworkBackupAgentEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.like(StrUtil.isNotBlank(request.getInstance()), NetworkBackupAgentEntity::getInstance, request.getInstance());
        wrapper.like(StrUtil.isNotBlank(request.getName()), NetworkBackupAgentEntity::getName, request.getName());
        wrapper.eq(StrUtil.isNotBlank(request.getAreaName()), NetworkBackupAgentEntity::getAreaName, request.getAreaName());
        wrapper.eq(StrUtil.isNotBlank(request.getStatus()), NetworkBackupAgentEntity::getStatus, request.getStatus());
        List<NetworkBackupAgentEntity> list = this.list(wrapper);
        List<NetworkBackupAgentRsp> dtoList = BeanUtil.copyToList(list, NetworkBackupAgentRsp.class);
        maskTokens(dtoList);
        ExcelUtils.exportExcelToTarget(response, null, "备份节点表", dtoList, NetworkBackupAgentExcel.class);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void delete(NetworkBackupAgentDeleteReq request) {
        if (request == null || request.getIds() == null || request.getIds().length == 0) {
            return;
        }
        Long[] ids = request.getIds();
        LambdaQueryWrapper<NetworkBackupDeviceEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.in(NetworkBackupDeviceEntity::getAgentId, Arrays.asList(ids));
        int used = Math.toIntExact(networkBackupDeviceMapper.selectCount(wrapper));
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
        LambdaQueryWrapper<NetworkBackupAgentEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.and((query) -> {
            if (StrUtil.isNotBlank(instance)) {
                query.or().eq(NetworkBackupAgentEntity::getInstance, instance);
            }
            if (StrUtil.isNotBlank(name)) {
                query.or().eq(NetworkBackupAgentEntity::getName, name);
            }
        });
        if (excludeId != null) {
            wrapper.ne(NetworkBackupAgentEntity::getId, excludeId);
        }
        return this.count(wrapper) > 0;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateStatus(Long[] ids, Integer status) {
        if (ids == null || ids.length == 0) {
            return;
        }
        NetworkBackupAgentEntity entity = new NetworkBackupAgentEntity();
        entity.setStatus(status);
        entity.setUpdater(SecurityUser.getUserId());
        entity.setUpdateDate(new Date());
        LambdaUpdateWrapper<NetworkBackupAgentEntity> wrapper = new LambdaUpdateWrapper<>();
        wrapper.in(NetworkBackupAgentEntity::getId, Arrays.asList(ids));
        this.update(entity, wrapper);
    }

    private Page<NetworkBackupAgentEntity> buildPage(NetworkBackupAgentPageReq request) {
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
        Page<NetworkBackupAgentEntity> page = new Page<>(curPage, limit);
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

    private void maskTokens(List<NetworkBackupAgentRsp> list) {
        if (list == null || list.isEmpty()) {
            return;
        }
        for (NetworkBackupAgentRsp dto : list) {
            maskToken(dto);
        }
    }

    private void fillOnlineStatus(List<NetworkBackupAgentRsp> list) {
        // online_status 已落库到 tb_network_backup_agent，BeanUtil 复制时会带上该字段
    }

    private void maskToken(NetworkBackupAgentRsp dto) {
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
            connection.setConnectTimeout(properties.getHttp().getTimeout().getBackupAgentHealthConnect());
            connection.setReadTimeout(properties.getHttp().getTimeout().getBackupAgentHealthRead());
            connection.connect();
            int code = connection.getResponseCode();
            if (code != 200) {
                return false;
            }
            try (InputStream in = connection.getInputStream()) {
                byte[] bytes = in.readAllBytes();
                String body = new String(bytes);
                for (String pattern : HEALTH_OK_PATTERNS) {
                    if (body.contains(pattern)) {
                        return true;
                    }
                }
                return false;
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
                port = properties.getBackup().getAgentDefaultPort();
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
            return "http://" + trimmed + ":" + properties.getBackup().getAgentDefaultPort() + "/healthz";
        }
    }
}
