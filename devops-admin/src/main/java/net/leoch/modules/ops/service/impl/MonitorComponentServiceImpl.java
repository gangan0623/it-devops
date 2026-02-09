package net.leoch.modules.ops.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.metadata.OrderItem;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import net.leoch.common.constant.Constant;
import net.leoch.common.exception.ServiceException;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.ConvertUtils;
import net.leoch.common.utils.JsonUtils;
import net.leoch.common.validator.ValidatorUtils;
import net.leoch.common.validator.group.AddGroup;
import net.leoch.common.validator.group.DefaultGroup;
import net.leoch.common.validator.group.UpdateGroup;
import net.leoch.modules.ops.mapper.MonitorComponentMapper;
import net.leoch.modules.ops.vo.req.*;
import net.leoch.modules.ops.vo.rsp.*;
import net.leoch.modules.ops.entity.MonitorComponentEntity;
import net.leoch.modules.ops.service.IMonitorComponentService;
import net.leoch.modules.security.user.SecurityUser;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 监控组件
 */
@Slf4j
@Service
public class MonitorComponentServiceImpl extends ServiceImpl<MonitorComponentMapper, MonitorComponentEntity> implements IMonitorComponentService {

    /** 允许排序的数据库列名白名单 */
    private static final Set<String> ALLOWED_ORDER_FIELDS = Set.of(
            "id", "name", "type", "ip", "port", "online_status",
            "version", "last_check_time", "create_date", "update_date");

    private static final String TYPE_PROMETHEUS = "prometheus";
    private static final String TYPE_VMALERT = "vmalert";
    private static final String TYPE_ALERTMANAGER = "alertmanager";
    private static final String TYPE_VICTORIAMETRICS = "victoriametrics";
    private static final String TYPE_BLACKBOX = "blackbox";

    @Override
    public PageData<MonitorComponentRsp> page(MonitorComponentPageReq request) {
        LambdaQueryWrapper<MonitorComponentEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.like(StrUtil.isNotBlank(request.getName()), MonitorComponentEntity::getName, request.getName());
        wrapper.eq(StrUtil.isNotBlank(request.getType()), MonitorComponentEntity::getType, normalizeType(request.getType()));
        wrapper.like(StrUtil.isNotBlank(request.getIp()), MonitorComponentEntity::getIp, request.getIp());
        Page<MonitorComponentEntity> page = buildPage(request);
        IPage<MonitorComponentEntity> result = this.page(page, wrapper);
        return new PageData<>(ConvertUtils.sourceToTarget(result.getRecords(), MonitorComponentRsp.class), result.getTotal());
    }

    @Override
    public MonitorComponentRsp get(MonitorComponentIdReq request) {
        if (request == null || request.getId() == null) {
            return null;
        }
        return ConvertUtils.sourceToTarget(this.getById(request.getId()), MonitorComponentRsp.class);
    }

    @Override
    public void save(MonitorComponentSaveReq request) {
        ValidatorUtils.validateEntity(request, AddGroup.class, DefaultGroup.class);
        validateUnique(request);
        if (request != null && request.getType() != null) {
            request.setType(normalizeType(request.getType()));
        }
        MonitorComponentEntity entity = ConvertUtils.sourceToTarget(request, MonitorComponentEntity.class);
        this.save(entity);
    }

    @Override
    public void update(MonitorComponentUpdateReq request) {
        ValidatorUtils.validateEntity(request, UpdateGroup.class, DefaultGroup.class);
        validateUnique(request);
        if (request != null && request.getType() != null) {
            request.setType(normalizeType(request.getType()));
        }
        MonitorComponentEntity entity = ConvertUtils.sourceToTarget(request, MonitorComponentEntity.class);
        this.updateById(entity);
    }

    @Override
    public void delete(MonitorComponentDeleteReq request) {
        if (request == null || request.getIds() == null || request.getIds().length == 0) {
            return;
        }
        this.removeByIds(Arrays.asList(request.getIds()));
    }

    @Override
    public boolean check(MonitorComponentCheckReq request) {
        if (request == null) {
            return false;
        }
        return existsByIpPortOrName(request.getIp(), request.getPort(), request.getName(), request.getId());
    }

    @Override
    public boolean probe(MonitorComponentProbeReq request) {
        if (request == null || request.getId() == null) {
            return false;
        }
        MonitorComponentEntity entity = this.getById(request.getId());
        if (entity == null) {
            return false;
        }
        boolean ok = probeByType(entity);
        updateProbeResult(entity.getId(), ok);
        return ok;
    }

    @Override
    public MonitorComponentRsp versionCheck(MonitorComponentVersionReq request) {
        if (request == null || request.getId() == null) {
            return null;
        }
        MonitorComponentEntity entity = this.getById(request.getId());
        if (entity == null) {
            return null;
        }
        String current = stripVersionPrefix(fetchVersion(entity));
        String latest = fetchLatestVersion(normalizeType(entity.getType()));
        Integer updateAvailable = null;
        if (StrUtil.isNotBlank(current) && StrUtil.isNotBlank(latest)) {
            updateAvailable = compareVersions(current, latest) < 0 ? 1 : 0;
        }
        LambdaUpdateWrapper<MonitorComponentEntity> wrapper = new LambdaUpdateWrapper<>();
        wrapper.eq(MonitorComponentEntity::getId, entity.getId());
        MonitorComponentEntity update = new MonitorComponentEntity();
        update.setVersion(current);
        update.setLatestVersion(latest);
        update.setUpdateAvailable(updateAvailable);
        update.setLastCheckTime(new Date());
        update.setUpdater(SecurityUser.getUserId());
        update.setUpdateDate(new Date());
        this.update(update, wrapper);
        MonitorComponentEntity refreshed = this.getById(entity.getId());
        return ConvertUtils.sourceToTarget(refreshed, MonitorComponentRsp.class);
    }

    @Override
    public List<MonitorComponentRsp> list(MonitorComponentListReq request) {
        LambdaQueryWrapper<MonitorComponentEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.orderByDesc(MonitorComponentEntity::getUpdateDate);
        List<MonitorComponentEntity> list = this.list(wrapper);
        return ConvertUtils.sourceToTarget(list, MonitorComponentRsp.class);
    }

    @Override
    public boolean existsByIpPortOrName(String ip, Integer port, String name, Long excludeId) {
        boolean hasIpPort = StrUtil.isNotBlank(ip) && port != null;
        boolean hasName = StrUtil.isNotBlank(name);
        if (!hasIpPort && !hasName) {
            return false;
        }
        LambdaQueryWrapper<MonitorComponentEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.and(query -> {
            if (hasIpPort) {
                query.or().eq(MonitorComponentEntity::getIp, ip).eq(MonitorComponentEntity::getPort, port);
            }
            if (hasName) {
                query.or().eq(MonitorComponentEntity::getName, name);
            }
        });
        if (excludeId != null) {
            wrapper.ne(MonitorComponentEntity::getId, excludeId);
        }
        return this.count(wrapper) > 0;
    }

    private Page<MonitorComponentEntity> buildPage(MonitorComponentPageReq request) {
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
        Page<MonitorComponentEntity> page = new Page<>(curPage, limit);
        if (request == null) {
            return page;
        }
        if (StrUtil.isNotBlank(request.getOrderField()) && StrUtil.isNotBlank(request.getOrder())) {
            String orderField = request.getOrderField();
            if (!ALLOWED_ORDER_FIELDS.contains(orderField)) {
                log.warn("[监控组件] 非法排序字段: {}", orderField);
            } else if (Constant.ASC.equalsIgnoreCase(request.getOrder())) {
                page.addOrder(OrderItem.asc(orderField));
            } else {
                page.addOrder(OrderItem.desc(orderField));
            }
        }
        return page;
    }

    private void validateUnique(MonitorComponentRsp dto) {
        if (dto != null && existsByIpPortOrName(dto.getIp(), dto.getPort(), dto.getName(), dto.getId())) {
            throw new ServiceException("IP端口或名称已存在");
        }
    }

    private String normalizeType(String type) {
        if (type == null) {
            return null;
        }
        return type.trim().toLowerCase();
    }

    private void updateProbeResult(Long id, boolean ok) {
        MonitorComponentEntity update = new MonitorComponentEntity();
        update.setOnlineStatus(ok ? 1 : 0);
        update.setLastCheckTime(new Date());
        update.setUpdater(SecurityUser.getUserId());
        update.setUpdateDate(new Date());
        LambdaUpdateWrapper<MonitorComponentEntity> wrapper = new LambdaUpdateWrapper<>();
        wrapper.eq(MonitorComponentEntity::getId, id);
        this.update(update, wrapper);
    }

    private boolean probeByType(MonitorComponentEntity entity) {
        String type = normalizeType(entity.getType());
        String base = buildBaseUrl(entity);
        if (StrUtil.isBlank(base)) {
            return false;
        }
        List<String> candidates = new ArrayList<>();
        if (TYPE_PROMETHEUS.equals(type)) {
            candidates.add(base + "/-/healthy");
            candidates.add(base + "/");
        } else if (TYPE_ALERTMANAGER.equals(type)) {
            candidates.add(base + "/-/healthy");
        } else if (TYPE_VICTORIAMETRICS.equals(type)) {
            candidates.add(base + "/health");
        } else if (TYPE_VMALERT.equals(type)) {
            candidates.add(base + "/health");
        } else if (TYPE_BLACKBOX.equals(type)) {
            candidates.add(base + "/-/healthy");
        } else {
            candidates.add(base);
        }
        for (String url : candidates) {
            if (httpOk(url)) {
                return true;
            }
        }
        return false;
    }

    private String fetchVersion(MonitorComponentEntity entity) {
        String type = normalizeType(entity.getType());
        String base = buildBaseUrl(entity);
        if (StrUtil.isBlank(base)) {
            return null;
        }
        if (TYPE_PROMETHEUS.equals(type)) {
            String json = httpGet(base + "/api/v1/status/buildinfo");
            String version = parseJsonVersion(json, "data", "version");
            if (StrUtil.isNotBlank(version)) {
                return version;
            }
        }
        if (TYPE_ALERTMANAGER.equals(type)) {
            String json = httpGet(base + "/api/v2/status");
            String version = parseJsonVersion(json, "versionInfo", "version");
            if (StrUtil.isNotBlank(version)) {
                return version;
            }
        }
        String metrics = httpGet(base + "/metrics");
        if (StrUtil.isBlank(metrics)) {
            return null;
        }
        if (TYPE_VMALERT.equals(type)) {
            String shortVersion = parseMetricsLabel(metrics, "vm_app_version", "short_version");
            return StrUtil.isNotBlank(shortVersion) ? shortVersion : parseMetricsLabel(metrics, "vm_app_version", "version");
        }
        if (TYPE_BLACKBOX.equals(type)) {
            return parseMetricsLabel(metrics, "blackbox_exporter_build_info", "version");
        }
        if (TYPE_VICTORIAMETRICS.equals(type)) {
            String shortVersion = parseMetricsLabel(metrics, "vm_app_version", "short_version");
            return StrUtil.isNotBlank(shortVersion) ? shortVersion : parseMetricsLabel(metrics, "vm_app_version", "version");
        }
        return null;
    }

    private String fetchLatestVersion(String type) {
        String repo;
        if (TYPE_PROMETHEUS.equals(type)) {
            repo = "prometheus/prometheus";
        } else if (TYPE_ALERTMANAGER.equals(type)) {
            repo = "prometheus/alertmanager";
        } else if (TYPE_BLACKBOX.equals(type)) {
            repo = "prometheus/blackbox_exporter";
        } else if (TYPE_VMALERT.equals(type)) {
            repo = "VictoriaMetrics/VictoriaMetrics";
        } else if (TYPE_VICTORIAMETRICS.equals(type)) {
            repo = "VictoriaMetrics/VictoriaMetrics";
        } else {
            return null;
        }
        String json = httpGet("https://api.github.com/repos/" + repo + "/releases/latest");
        String tag = parseJsonVersion(json, "tag_name");
        if (StrUtil.isBlank(tag)) {
            return null;
        }
        return stripVersionPrefix(tag);
    }

    private String stripVersionPrefix(String version) {
        if (StrUtil.isBlank(version)) {
            return version;
        }
        return version.startsWith("v") ? version.substring(1) : version;
    }

    private String parseJsonVersion(String json, String... keys) {
        if (StrUtil.isBlank(json)) {
            return null;
        }
        try {
            Object current = JsonUtils.parseObject(json, Map.class);
            for (String key : keys) {
                if (!(current instanceof Map)) {
                    return null;
                }
                current = ((Map<?, ?>) current).get(key);
            }
            return current == null ? null : String.valueOf(current);
        } catch (Exception ignore) {
            return null;
        }
    }

    private String parseMetricsLabel(String metrics, String metricName, String labelKey) {
        if (StrUtil.isBlank(metrics) || StrUtil.isBlank(metricName)) {
            return null;
        }
        String[] lines = metrics.split("\\r?\\n");
        for (String line : lines) {
            if (!line.startsWith(metricName + "{")) {
                continue;
            }
            String key = (labelKey == null || labelKey.isBlank()) ? "version" : labelKey;
            int braceStart = line.indexOf('{');
            int braceEnd = line.indexOf('}');
            if (braceStart < 0 || braceEnd <= braceStart) {
                continue;
            }
            String labels = line.substring(braceStart + 1, braceEnd);
            Pattern pattern = Pattern.compile("(^|,)\\s*" + Pattern.quote(key) + "=\"([^\"]*)\"");
            Matcher matcher = pattern.matcher(labels);
            if (matcher.find()) {
                return matcher.group(2);
            }
        }
        return null;
    }

    private int compareVersions(String v1, String v2) {
        String a = normalizeVersion(v1);
        String b = normalizeVersion(v2);
        String[] p1 = a.split("\\.");
        String[] p2 = b.split("\\.");
        int len = Math.max(p1.length, p2.length);
        for (int i = 0; i < len; i++) {
            int n1 = i < p1.length ? parseInt(p1[i]) : 0;
            int n2 = i < p2.length ? parseInt(p2[i]) : 0;
            if (n1 != n2) {
                return Integer.compare(n1, n2);
            }
        }
        return 0;
    }

    private String normalizeVersion(String value) {
        if (value == null) {
            return "";
        }
        String v = value.trim();
        if (v.startsWith("v")) {
            v = v.substring(1);
        }
        int plus = v.indexOf('+');
        if (plus > 0) {
            v = v.substring(0, plus);
        }
        int dash = v.indexOf('-');
        if (dash > 0) {
            v = v.substring(0, dash);
        }
        return v;
    }

    private int parseInt(String value) {
        try {
            return Integer.parseInt(value.replaceAll("\\D", ""));
        } catch (Exception ignore) {
            return 0;
        }
    }

    private boolean httpOk(String url) {
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

    private String httpGet(String url) {
        HttpURLConnection connection = null;
        try {
            connection = (HttpURLConnection) new URL(url).openConnection();
            connection.setRequestMethod("GET");
            connection.setConnectTimeout(5000);
            connection.setReadTimeout(8000);
            if (url.contains("api.github.com")) {
                connection.setRequestProperty("User-Agent", "devops-monitor");
            }
            int code = connection.getResponseCode();
            if (code != 200) {
                return null;
            }
            try (InputStream in = connection.getInputStream()) {
                byte[] bytes = in.readAllBytes();
                return new String(bytes, StandardCharsets.UTF_8);
            }
        } catch (Exception ignore) {
            return null;
        } finally {
            if (connection != null) {
                connection.disconnect();
            }
        }
    }

    private String buildBaseUrl(MonitorComponentEntity entity) {
        if (entity == null) {
            return null;
        }
        if (StrUtil.isNotBlank(entity.getWebUrl())) {
            return trimRight(entity.getWebUrl());
        }
        if (StrUtil.isBlank(entity.getIp())) {
            return null;
        }
        String host = entity.getIp().trim();
        if (entity.getPort() != null) {
            return "http://" + host + ":" + entity.getPort();
        }
        return "http://" + host;
    }

    private String trimRight(String url) {
        String value = url.trim();
        while (value.endsWith("/")) {
            value = value.substring(0, value.length() - 1);
        }
        return value;
    }
}
