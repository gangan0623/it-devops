package net.leoch.modules.ops.service.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import net.leoch.common.base.Constant;
import net.leoch.common.data.page.PageData;
import net.leoch.common.exception.ServiceException;
import net.leoch.framework.config.ops.ZabbixConfig;
import net.leoch.modules.ops.entity.BackupAgentEntity;
import net.leoch.modules.ops.entity.NetworkBackupDeviceEntity;
import net.leoch.modules.ops.entity.NetworkHostEntity;
import net.leoch.modules.ops.mapper.BackupAgentMapper;
import net.leoch.modules.ops.mapper.NetworkBackupDeviceMapper;
import net.leoch.modules.ops.mapper.NetworkHostMapper;
import net.leoch.modules.ops.service.INetworkHostService;
import net.leoch.modules.ops.service.ZabbixClient;
import net.leoch.modules.ops.service.ZabbixConfigService;
import net.leoch.modules.ops.vo.req.NetworkHostBackupSaveReq;
import net.leoch.modules.ops.vo.req.NetworkHostPageReq;
import net.leoch.modules.ops.vo.rsp.NetworkInterfaceDetailRsp;
import net.leoch.modules.ops.vo.rsp.NetworkHostBackupDetailRsp;
import net.leoch.modules.ops.vo.rsp.NetworkHostRsp;
import net.leoch.modules.ops.vo.rsp.NetworkInterfaceTrendRsp;
import net.leoch.modules.ops.vo.rsp.OpsHostStatusSummaryRsp;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class NetworkHostServiceImpl extends ServiceImpl<NetworkHostMapper, NetworkHostEntity> implements INetworkHostService {
    private static final Set<String> ALLOWED_ORDER_FIELDS = Set.of(
            "id", "instance", "name", "area_name", "group_name", "device_model",
            "status", "collection_status", "online_status", "missing_count",
            "last_seen_time", "last_sync_time", "update_date", "create_date"
    );
    private static final Pattern BRACKET_PATTERN = Pattern.compile("\\[(.+?)]");
    private static final Pattern INTERFACE_NAME_PATTERN = Pattern.compile("接口\\s*([^：:]+)");

    private final ZabbixConfigService zabbixConfigService;
    private final ZabbixClient zabbixClient;
    private final NetworkBackupDeviceMapper networkBackupDeviceMapper;
    private final BackupAgentMapper backupAgentMapper;

    @Override
    public PageData<NetworkHostRsp> page(NetworkHostPageReq request) {
        LambdaQueryWrapper<NetworkHostEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.like(StrUtil.isNotBlank(request.getInstance()), NetworkHostEntity::getInstance, request.getInstance());
        wrapper.like(StrUtil.isNotBlank(request.getName()), NetworkHostEntity::getName, request.getName());
        wrapper.eq(StrUtil.isNotBlank(request.getAreaName()), NetworkHostEntity::getAreaName, request.getAreaName());
        wrapper.eq(StrUtil.isNotBlank(request.getGroupName()), NetworkHostEntity::getGroupName, request.getGroupName());
        wrapper.eq(StrUtil.isNotBlank(request.getDeviceModel()), NetworkHostEntity::getDeviceModel, request.getDeviceModel());
        wrapper.eq(StrUtil.isNotBlank(request.getStatus()), NetworkHostEntity::getStatus, request.getStatus());
        wrapper.eq(StrUtil.isNotBlank(request.getCollectionStatus()), NetworkHostEntity::getCollectionStatus, request.getCollectionStatus());
        wrapper.eq(StrUtil.isNotBlank(request.getOnlineStatus()), NetworkHostEntity::getOnlineStatus, request.getOnlineStatus());
        if ("1".equals(request.getBackupStatus())) {
            wrapper.apply("exists (select 1 from tb_network_backup_device nbd where nbd.network_host_id = tb_network_host.id and nbd.backup_enabled = 1)");
        } else if ("0".equals(request.getBackupStatus())) {
            wrapper.apply("not exists (select 1 from tb_network_backup_device nbd where nbd.network_host_id = tb_network_host.id and nbd.backup_enabled = 1)");
        }

        Page<NetworkHostEntity> page = request.buildPage();
        if (StrUtil.isNotBlank(request.getOrderField()) && ALLOWED_ORDER_FIELDS.contains(request.getOrderField())) {
            if (Constant.ASC.equalsIgnoreCase(request.getOrder())) {
                page.addOrder(com.baomidou.mybatisplus.core.metadata.OrderItem.asc(request.getOrderField()));
            } else {
                page.addOrder(com.baomidou.mybatisplus.core.metadata.OrderItem.desc(request.getOrderField()));
            }
        } else {
            page.addOrder(com.baomidou.mybatisplus.core.metadata.OrderItem.desc("update_date"));
        }
        IPage<NetworkHostEntity> result = this.page(page, wrapper);
        List<NetworkHostRsp> rspList = BeanUtil.copyToList(result.getRecords(), NetworkHostRsp.class);
        fillBackupStatus(rspList);
        return new PageData<>(rspList, result.getTotal());
    }

    @Override
    public OpsHostStatusSummaryRsp summary(NetworkHostPageReq request) {
        LambdaQueryWrapper<NetworkHostEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.select(NetworkHostEntity::getStatus, NetworkHostEntity::getOnlineStatus);
        List<NetworkHostEntity> list = this.list(wrapper);
        OpsHostStatusSummaryRsp summary = new OpsHostStatusSummaryRsp();
        summary.setTotalCount((long) list.size());
        for (NetworkHostEntity item : list) {
            if (item == null) {
                continue;
            }
            Integer status = item.getStatus();
            if (Integer.valueOf(1).equals(status)) {
                summary.setEnabledCount(summary.getEnabledCount() + 1);
            } else if (Integer.valueOf(0).equals(status)) {
                summary.setDisabledCount(summary.getDisabledCount() + 1);
            }

            Integer onlineStatus = item.getOnlineStatus();
            if (Integer.valueOf(1).equals(onlineStatus)) {
                summary.setOnlineCount(summary.getOnlineCount() + 1);
            } else if (Integer.valueOf(0).equals(onlineStatus)) {
                summary.setOfflineCount(summary.getOfflineCount() + 1);
            } else {
                summary.setUnknownCount(summary.getUnknownCount() + 1);
            }
        }
        return summary;
    }

    @Override
    public NetworkHostBackupDetailRsp backupDetail(Long networkHostId) {
        if (networkHostId == null) {
            return null;
        }
        NetworkHostEntity host = this.getById(networkHostId);
        if (host == null) {
            return null;
        }
        NetworkBackupDeviceEntity backup = networkBackupDeviceMapper.selectOne(new LambdaQueryWrapper<NetworkBackupDeviceEntity>()
                .eq(NetworkBackupDeviceEntity::getNetworkHostId, networkHostId)
                .last("limit 1"));

        NetworkHostBackupDetailRsp rsp = new NetworkHostBackupDetailRsp();
        rsp.setNetworkHostId(host.getId());
        rsp.setInstance(host.getInstance());
        rsp.setName(host.getName());
        rsp.setAreaName(host.getAreaName());
        rsp.setGroupName(host.getGroupName());
        rsp.setDeviceModel(host.getDeviceModel());
        rsp.setHostStatus(host.getStatus());
        if (backup != null) {
            rsp.setBackupEnabled(backup.getBackupEnabled());
            rsp.setUsername(backup.getUsername());
            rsp.setPassword(StrUtil.isBlank(backup.getPassword()) ? "" : "******");
            rsp.setAgentId(backup.getAgentId());
            rsp.setLastBackupTime(backup.getLastBackupTime());
            rsp.setLastBackupStatus(backup.getLastBackupStatus());
            rsp.setLastBackupMessage(backup.getLastBackupMessage());
            if (backup.getAgentId() != null) {
                BackupAgentEntity agent = backupAgentMapper.selectById(backup.getAgentId());
                rsp.setBackupAgentName(agent == null ? "" : agent.getName());
            }
        } else {
            rsp.setBackupEnabled(0);
            rsp.setPassword("");
        }
        rsp.setCanBackup(canBackup(host.getStatus(), rsp.getBackupEnabled()) ? 1 : 0);
        return rsp;
    }

    @Override
    public void saveBackupDetail(NetworkHostBackupSaveReq request) {
        if (request == null || request.getNetworkHostId() == null) {
            throw new ServiceException("网络设备ID不能为空");
        }
        NetworkHostEntity host = this.getById(request.getNetworkHostId());
        if (host == null) {
            throw new ServiceException("网络设备不存在");
        }
        NetworkBackupDeviceEntity existing = networkBackupDeviceMapper.selectOne(new LambdaQueryWrapper<NetworkBackupDeviceEntity>()
                .eq(NetworkBackupDeviceEntity::getNetworkHostId, request.getNetworkHostId())
                .last("limit 1"));
        Integer desiredEnabled = Integer.valueOf(1).equals(request.getBackupEnabled()) ? 1 : 0;
        Integer finalEnabled = Integer.valueOf(1).equals(host.getStatus()) ? desiredEnabled : 0;
        String mergedUsername = StrUtil.isBlank(request.getUsername())
                ? (existing == null ? "" : StrUtil.nullToDefault(existing.getUsername(), ""))
                : StrUtil.trimToEmpty(request.getUsername());
        String mergedPassword = StrUtil.isBlank(request.getPassword())
                ? (existing == null ? "" : StrUtil.nullToDefault(existing.getPassword(), ""))
                : StrUtil.trimToEmpty(request.getPassword());
        Long mergedAgentId = request.getAgentId() == null ? (existing == null ? null : existing.getAgentId()) : request.getAgentId();
        if (Integer.valueOf(1).equals(finalEnabled)
                && (StrUtil.isBlank(mergedUsername)
                || StrUtil.isBlank(mergedPassword)
                || mergedAgentId == null)) {
            throw new ServiceException("启用备份时，账号、密码、备份节点不能为空");
        }

        NetworkBackupDeviceEntity entity = new NetworkBackupDeviceEntity();
        if (existing != null) {
            entity.setId(existing.getId());
        }
        entity.setNetworkHostId(host.getId());
        entity.setInstance(host.getInstance());
        entity.setName(host.getName());
        entity.setAreaName(host.getAreaName());
        entity.setGroupName(host.getGroupName());
        entity.setDeviceModel(host.getDeviceModel());
        entity.setStatus(host.getStatus());
        entity.setUsername(mergedUsername);
        entity.setPassword(mergedPassword);
        entity.setAgentId(mergedAgentId);
        entity.setBackupEnabled(finalEnabled);
        if (existing == null) {
            networkBackupDeviceMapper.insert(entity);
        } else {
            networkBackupDeviceMapper.updateById(entity);
        }
    }

    @Override
    public List<NetworkInterfaceDetailRsp> interfaceDetails(String instance, boolean includeZeroTraffic) {
        if (StrUtil.isBlank(instance)) {
            return List.of();
        }
        ZabbixConfig config = zabbixConfigService.getConfig();
        if (config == null || StrUtil.isBlank(config.getUrl())) {
            return List.of();
        }
        List<Map<String, Object>> itemRows = zabbixClient.getInterfaceItemsByHostIp(config, instance.trim());
        if (itemRows.isEmpty()) {
            return List.of();
        }

        Map<String, InterfaceAccumulator> map = new LinkedHashMap<>();
        for (Map<String, Object> row : itemRows) {
            String key = StrUtil.nullToDefault((String) row.get("key_"), "");
            String name = StrUtil.nullToDefault((String) row.get("name"), "");
            String index = extractInterfaceIndex(key, name);
            if (StrUtil.isBlank(index)) {
                continue;
            }
            InterfaceAccumulator acc = map.computeIfAbsent(index, k -> new InterfaceAccumulator(k));
            acc.apply(key, name, StrUtil.nullToDefault((String) row.get("lastvalue"), ""), row.get("tags"));
        }

        List<NetworkInterfaceDetailRsp> list = new ArrayList<>();
        for (InterfaceAccumulator acc : map.values()) {
            if (!includeZeroTraffic && acc.inTrafficKbps <= 0D && acc.outTrafficKbps <= 0D) {
                continue;
            }
            NetworkInterfaceDetailRsp rsp = new NetworkInterfaceDetailRsp();
            rsp.setInterfaceIndex(acc.index);
            rsp.setInterfaceName(StrUtil.blankToDefault(acc.interfaceName, "ifIndex-" + acc.index));
            rsp.setStatus(acc.statusText);
            rsp.setInTrafficKbps(round2(acc.inTrafficKbps));
            rsp.setOutTrafficKbps(round2(acc.outTrafficKbps));
            rsp.setSpeedMbps(round2(acc.speedMbps));
            rsp.setInDropError((long) acc.inDiscards + "/" + (long) acc.inErrors);
            rsp.setOutDropError((long) acc.outDiscards + "/" + (long) acc.outErrors);
            list.add(rsp);
        }
        list.sort((a, b) -> compareInterfaceIndex(a.getInterfaceIndex(), b.getInterfaceIndex()));
        return list;
    }

    @Override
    public NetworkInterfaceTrendRsp interfaceTrend(String instance, String interfaceIndex, Long timeFrom, Long timeTill) {
        NetworkInterfaceTrendRsp rsp = new NetworkInterfaceTrendRsp();
        rsp.setInterfaceIndex(StrUtil.nullToDefault(interfaceIndex, ""));
        rsp.setTimestamps(List.of());
        rsp.setInBitrateKbps(List.of());
        rsp.setOutBitrateKbps(List.of());

        if (StrUtil.isBlank(instance) || StrUtil.isBlank(interfaceIndex) || timeFrom == null || timeTill == null || timeTill < timeFrom) {
            return rsp;
        }
        ZabbixConfig config = zabbixConfigService.getConfig();
        if (config == null || StrUtil.isBlank(config.getUrl())) {
            return rsp;
        }

        Map<String, List<Map<String, Object>>> metricHistory = zabbixClient.getInterfaceMetricHistoryByHostIp(
                config, instance.trim(), interfaceIndex.trim(), timeFrom, timeTill
        );
        if (metricHistory.isEmpty()) {
            return rsp;
        }

        TreeMap<Long, TrendPoint> timeline = new TreeMap<>();
        mergeSeries(timeline, metricHistory.get("in"), TrendPoint::setInBitrateKbps, true);
        mergeSeries(timeline, metricHistory.get("out"), TrendPoint::setOutBitrateKbps, true);
        if (timeline.isEmpty()) {
            return rsp;
        }

        List<Long> timestamps = new ArrayList<>(timeline.size());
        List<Double> in = new ArrayList<>(timeline.size());
        List<Double> out = new ArrayList<>(timeline.size());

        for (Map.Entry<Long, TrendPoint> entry : timeline.entrySet()) {
            TrendPoint point = entry.getValue();
            timestamps.add(entry.getKey());
            in.add(round2(point.inBitrateKbps));
            out.add(round2(point.outBitrateKbps));
        }
        rsp.setTimestamps(timestamps);
        rsp.setInBitrateKbps(in);
        rsp.setOutBitrateKbps(out);
        return rsp;
    }

    private void mergeSeries(TreeMap<Long, TrendPoint> timeline,
                             List<Map<String, Object>> rows,
                             java.util.function.BiConsumer<TrendPoint, Double> setter,
                             boolean kbps) {
        if (rows == null || rows.isEmpty()) {
            return;
        }
        for (Map<String, Object> row : rows) {
            long clock = parseLongDefault(row.get("clock"), -1L);
            if (clock <= 0) {
                continue;
            }
            double value = parseDouble(String.valueOf(row.get("value")));
            if (kbps) {
                value = value / 1000D;
            }
            TrendPoint point = timeline.computeIfAbsent(clock, k -> new TrendPoint());
            setter.accept(point, value);
        }
    }

    private long parseLongDefault(Object value, long def) {
        if (value == null) {
            return def;
        }
        try {
            return Long.parseLong(String.valueOf(value));
        } catch (Exception ignored) {
            return def;
        }
    }

    private String extractInterfaceIndex(String key, String name) {
        String inBracket = extractBracketFirstPart(key);
        if (StrUtil.isNotBlank(inBracket)) {
            return normalizeInterfaceIdentifier(inBracket);
        }
        return null;
    }

    private String normalizeInterfaceIdentifier(String raw) {
        String text = StrUtil.blankToDefault(raw, "").trim();
        if (StrUtil.isBlank(text)) {
            return null;
        }
        if ((text.startsWith("\"") && text.endsWith("\"")) || (text.startsWith("'") && text.endsWith("'"))) {
            text = text.substring(1, text.length() - 1).trim();
        }
        int dot = text.lastIndexOf('.');
        if (dot > 0 && dot < text.length() - 1) {
            String suffix = text.substring(dot + 1).trim();
            if (suffix.matches("^\\d+$")) {
                return suffix;
            }
        }
        return text;
    }

    private void fillBackupStatus(List<NetworkHostRsp> list) {
        if (list == null || list.isEmpty()) {
            return;
        }
        List<Long> hostIds = list.stream()
                .map(NetworkHostRsp::getId)
                .filter(Objects::nonNull)
                .distinct()
                .toList();
        if (hostIds.isEmpty()) {
            return;
        }
        List<NetworkBackupDeviceEntity> backups = networkBackupDeviceMapper.selectList(new LambdaQueryWrapper<NetworkBackupDeviceEntity>()
                .in(NetworkBackupDeviceEntity::getNetworkHostId, hostIds));
        if (backups.isEmpty()) {
            for (NetworkHostRsp item : list) {
                item.setBackupEnabled(0);
                item.setBackupStatus(0);
            }
            return;
        }
        Map<Long, NetworkBackupDeviceEntity> backupMap = new HashMap<>();
        for (NetworkBackupDeviceEntity backup : backups) {
            if (backup == null || backup.getNetworkHostId() == null || backupMap.containsKey(backup.getNetworkHostId())) {
                continue;
            }
            backupMap.put(backup.getNetworkHostId(), backup);
        }
        List<Long> agentIds = backups.stream()
                .map(NetworkBackupDeviceEntity::getAgentId)
                .filter(Objects::nonNull)
                .distinct()
                .collect(Collectors.toList());
        Map<Long, String> agentNameMap = new HashMap<>();
        if (!agentIds.isEmpty()) {
            List<BackupAgentEntity> agents = backupAgentMapper.selectBatchIds(agentIds);
            for (BackupAgentEntity agent : agents) {
                if (agent != null && agent.getId() != null) {
                    agentNameMap.put(agent.getId(), agent.getName());
                }
            }
        }
        for (NetworkHostRsp item : list) {
            NetworkBackupDeviceEntity backup = backupMap.get(item.getId());
            if (backup == null) {
                item.setBackupEnabled(0);
                item.setBackupStatus(0);
                continue;
            }
            item.setBackupEnabled(backup.getBackupEnabled());
            item.setBackupAgentId(backup.getAgentId());
            item.setBackupAgentName(backup.getAgentId() == null ? "" : StrUtil.nullToDefault(agentNameMap.get(backup.getAgentId()), ""));
            item.setBackupStatus(canBackup(item.getStatus(), backup.getBackupEnabled()) ? 1 : 0);
        }
    }

    private boolean canBackup(Integer hostStatus, Integer backupEnabled) {
        return Integer.valueOf(1).equals(hostStatus) && Integer.valueOf(1).equals(backupEnabled);
    }

    private String extractBracketFirstPart(String key) {
        if (StrUtil.isBlank(key)) {
            return null;
        }
        Matcher m = BRACKET_PATTERN.matcher(key);
        if (!m.find()) {
            return null;
        }
        String inside = StrUtil.blankToDefault(m.group(1), "");
        int comma = inside.indexOf(",");
        return comma >= 0 ? inside.substring(0, comma).trim() : inside.trim();
    }

    private String extractDisplayInterfaceName(String itemName) {
        if (StrUtil.isBlank(itemName)) {
            return null;
        }
        Matcher matcher = INTERFACE_NAME_PATTERN.matcher(itemName);
        if (!matcher.find()) {
            return null;
        }
        String raw = StrUtil.nullToDefault(matcher.group(1), "").trim();
        if (StrUtil.isBlank(raw)) {
            return null;
        }
        String cleaned = raw.replace("()", "").trim();
        return StrUtil.isBlank(cleaned) ? null : cleaned;
    }

    private String extractInterfaceNameFromTags(Object tagsObj) {
        if (!(tagsObj instanceof List<?> tagList) || tagList.isEmpty()) {
            return null;
        }
        for (Object tag : tagList) {
            if (!(tag instanceof Map<?, ?> map)) {
                continue;
            }
            String tagName = map.get("tag") == null ? "" : String.valueOf(map.get("tag")).toLowerCase();
            String tagValue = map.get("value") == null ? "" : String.valueOf(map.get("value")).trim();
            if (StrUtil.isBlank(tagValue)) {
                continue;
            }
            if (tagName.contains("ifname") || tagName.contains("ifdescr") || tagName.contains("ifalias")
                    || tagName.contains("interface") || tagName.contains("port")) {
                return tagValue.replace("()", "").trim();
            }
        }
        return null;
    }

    private double parseDouble(String v) {
        if (StrUtil.isBlank(v)) {
            return 0D;
        }
        try {
            return Double.parseDouble(v);
        } catch (Exception ignored) {
            return 0D;
        }
    }

    private int compareInterfaceIndex(String a, String b) {
        String left = StrUtil.nullToDefault(a, "");
        String right = StrUtil.nullToDefault(b, "");
        boolean leftNumeric = left.matches("^\\d+$");
        boolean rightNumeric = right.matches("^\\d+$");
        if (leftNumeric && rightNumeric) {
            return Integer.compare(Integer.parseInt(left), Integer.parseInt(right));
        }
        if (leftNumeric) {
            return -1;
        }
        if (rightNumeric) {
            return 1;
        }
        return left.compareToIgnoreCase(right);
    }

    private double round2(double value) {
        return Math.round(value * 100D) / 100D;
    }

    private class InterfaceAccumulator {
        private final String index;
        private String interfaceName;
        private String statusText = "未知";
        private double inTrafficKbps = 0D;
        private double outTrafficKbps = 0D;
        private double speedMbps = 0D;
        private double inDiscards = 0D;
        private double outDiscards = 0D;
        private double inErrors = 0D;
        private double outErrors = 0D;

        InterfaceAccumulator(String index) {
            this.index = index;
        }

        void apply(String key, String name, String lastValue, Object tagsObj) {
            String lowerKey = StrUtil.blankToDefault(key, "").toLowerCase();
            if (lowerKey.startsWith("ifname[") || lowerKey.startsWith("ifdescr[") || lowerKey.startsWith("ifalias[")) {
                if (StrUtil.isNotBlank(lastValue)) {
                    this.interfaceName = lastValue;
                }
                return;
            }
            if (StrUtil.isBlank(this.interfaceName)) {
                String tagName = extractInterfaceNameFromTags(tagsObj);
                if (StrUtil.isNotBlank(tagName)) {
                    this.interfaceName = tagName;
                }
            }
            if (StrUtil.isBlank(this.interfaceName)) {
                String parsedName = extractDisplayInterfaceName(name);
                if (StrUtil.isNotBlank(parsedName)) {
                    this.interfaceName = parsedName;
                }
            }
            double numeric = parseDouble(lastValue);
            if (lowerKey.startsWith("net.if.in[")) {
                this.inTrafficKbps = numeric / 1000D;
            } else if (lowerKey.startsWith("net.if.out[")) {
                this.outTrafficKbps = numeric / 1000D;
            } else if (lowerKey.startsWith("net.if.speed[")) {
                this.speedMbps = numeric / 1_000_000D;
            } else if (lowerKey.startsWith("net.if.in.discards[")) {
                this.inDiscards = numeric;
            } else if (lowerKey.startsWith("net.if.out.discards[")) {
                this.outDiscards = numeric;
            } else if (lowerKey.startsWith("net.if.in.errors[")) {
                this.inErrors = numeric;
            } else if (lowerKey.startsWith("net.if.out.errors[")) {
                this.outErrors = numeric;
            } else if (lowerKey.startsWith("net.if.status[")) {
                this.statusText = "1".equals(lastValue) ? "UP" : ("2".equals(lastValue) ? "DOWN" : "未知");
            } else if (lowerKey.startsWith("ifoperstatus[")) {
                this.statusText = "1".equals(lastValue) ? "UP" : ("2".equals(lastValue) ? "DOWN" : "未知");
            }
        }
    }

    private static class TrendPoint {
        private double inBitrateKbps = 0D;
        private double outBitrateKbps = 0D;

        void setInBitrateKbps(Double value) {
            this.inBitrateKbps = value == null ? 0D : value;
        }

        void setOutBitrateKbps(Double value) {
            this.outBitrateKbps = value == null ? 0D : value;
        }
    }
}
