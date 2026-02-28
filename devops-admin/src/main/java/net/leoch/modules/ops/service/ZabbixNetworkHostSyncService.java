package net.leoch.modules.ops.service;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.integration.security.SecurityUser;
import net.leoch.modules.ops.entity.NetworkBackupDeviceEntity;
import net.leoch.modules.ops.entity.NetworkHostEntity;
import net.leoch.modules.ops.mapper.NetworkBackupDeviceMapper;
import net.leoch.modules.ops.service.impl.NetworkHostServiceImpl;
import net.leoch.modules.ops.vo.rsp.ZabbixNetworkHostSyncRsp;
import net.leoch.modules.sys.service.ZabbixNetworkDeviceMappingService;
import net.leoch.modules.sys.vo.req.ZabbixNetworkDeviceMappingPreviewReq;
import net.leoch.modules.sys.vo.rsp.ZabbixNetworkDeviceMappingPreviewRsp;
import net.leoch.modules.sys.vo.rsp.ZabbixNetworkDeviceMappingRsp;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

@Slf4j
@Service
@RequiredArgsConstructor
public class ZabbixNetworkHostSyncService {
    private static final int MISSING_DISABLE_THRESHOLD = 3;
    private final ZabbixConfigService zabbixConfigService;
    private final ZabbixClient zabbixClient;
    private final ZabbixNetworkDeviceMappingService mappingService;
    private final NetworkHostServiceImpl networkHostService;
    private final NetworkBackupDeviceMapper networkBackupDeviceMapper;

    @Transactional(rollbackFor = Exception.class)
    public ZabbixNetworkHostSyncRsp sync() {
        ZabbixNetworkHostSyncRsp rsp = new ZabbixNetworkHostSyncRsp();
        rsp.setMissingThreshold(MISSING_DISABLE_THRESHOLD);

        var zabbixConfig = zabbixConfigService.getConfig();
        ZabbixNetworkDeviceMappingRsp mappingConfig = mappingService.getConfig();
        List<String> selectedGroupIds = mappingConfig.getSelectedHostGroupIds();
        if (CollUtil.isEmpty(selectedGroupIds)) {
            log.warn("[Zabbix网络设备同步] 未配置主机群组映射");
            rsp.setSyncSuccess(0);
            rsp.setMessage("未配置主机群组映射");
            return rsp;
        }

        // 同步前先校验Zabbix连通性，失败时不做任何禁用/缺失累计，避免误判。
        try {
            zabbixClient.testConnection(zabbixConfig);
        } catch (Exception e) {
            log.warn("[Zabbix网络设备同步] Zabbix连接校验失败，本次跳过数据变更", e);
            rsp.setSyncSuccess(0);
            rsp.setMessage("Zabbix连接异常，本次未执行缺失禁用");
            return rsp;
        }

        ZabbixNetworkDeviceMappingPreviewReq previewReq = new ZabbixNetworkDeviceMappingPreviewReq();
        previewReq.setSelectedHostGroupIds(new ArrayList<>(selectedGroupIds));
        previewReq.setCategoryGroupRules(mappingConfig.getCategoryGroupRules());
        previewReq.setAreaKeywordRules(mappingConfig.getAreaKeywordRules());
        ZabbixNetworkDeviceMappingPreviewRsp preview = mappingService.preview(previewReq);
        Map<String, ZabbixNetworkDeviceMappingPreviewRsp.GroupPreviewItem> groupRuleMap = preview.getGroupList().stream()
                .filter(Objects::nonNull)
                .collect(Collectors.toMap(ZabbixNetworkDeviceMappingPreviewRsp.GroupPreviewItem::getGroupId, it -> it, (a, b) -> a, LinkedHashMap::new));
        rsp.setUnmatchedAreas(new ArrayList<>(preview.getUnmatchedAreas()));

        Map<String, String> templateModelMap = new LinkedHashMap<>();
        if (CollUtil.isNotEmpty(mappingConfig.getTemplateModelRules())) {
            for (var rule : mappingConfig.getTemplateModelRules()) {
                if (rule == null || StrUtil.isBlank(rule.getTemplateName())) {
                    continue;
                }
                templateModelMap.put(rule.getTemplateName().trim(), StrUtil.blankToDefault(rule.getDeviceModel(), ""));
            }
        }

        List<Map<String, Object>> hosts = zabbixClient.getHostsForNetworkSync(zabbixConfig, selectedGroupIds);
        rsp.setTotalFetched(hosts.size());
        if (hosts.isEmpty()) {
            markMissing(Set.of(), rsp);
            return rsp;
        }

        List<String> hostIds = hosts.stream()
                .map(h -> h.get("hostid"))
                .filter(Objects::nonNull)
                .map(String::valueOf)
                .toList();
        Map<String, Map<String, Map<String, Object>>> icmpMap = zabbixClient.getIcmpItemsByHostIds(zabbixConfig, hostIds);

        Map<String, NetworkHostEntity> existingByIp = networkHostService.list(new LambdaQueryWrapper<NetworkHostEntity>()
                        .select(NetworkHostEntity::getId, NetworkHostEntity::getInstance, NetworkHostEntity::getStatus, NetworkHostEntity::getMissingCount))
                .stream()
                .filter(Objects::nonNull)
                .filter(it -> StrUtil.isNotBlank(it.getInstance()))
                .collect(Collectors.toMap(NetworkHostEntity::getInstance, it -> it, (a, b) -> a, LinkedHashMap::new));
        Map<Long, NetworkBackupDeviceEntity> backupByHostId = networkBackupDeviceMapper.selectList(new LambdaQueryWrapper<NetworkBackupDeviceEntity>()
                        .select(NetworkBackupDeviceEntity::getId, NetworkBackupDeviceEntity::getNetworkHostId))
                .stream()
                .filter(Objects::nonNull)
                .filter(it -> it.getNetworkHostId() != null)
                .collect(Collectors.toMap(NetworkBackupDeviceEntity::getNetworkHostId, it -> it, (a, b) -> a, LinkedHashMap::new));

        Set<String> syncedIps = new LinkedHashSet<>();
        Long operatorId = Optional.ofNullable(SecurityUser.getUserId()).orElse(0L);
        Date now = new Date();

        for (Map<String, Object> host : hosts) {
            String hostId = str(host.get("hostid"));
            String hostName = StrUtil.blankToDefault(str(host.get("host")), str(host.get("name")));
            String ip = extractHostIp(host.get("interfaces"));
            if (StrUtil.isBlank(ip)) {
                rsp.setSkipped(rsp.getSkipped() + 1);
                continue;
            }
            syncedIps.add(ip);

            HostGroupResolved resolved = resolveHostGroup(host.get("hostgroups"), groupRuleMap);
            if (resolved == null || StrUtil.isBlank(resolved.areaName) || StrUtil.isBlank(resolved.deviceGroup)) {
                rsp.setSkipped(rsp.getSkipped() + 1);
                continue;
            }

            String deviceModel = resolveDeviceModel(host.get("parentTemplates"), templateModelMap);
            Map<String, Map<String, Object>> itemByKey = icmpMap.getOrDefault(hostId, Map.of());
            String icmpPing = str(itemByKey.getOrDefault("icmpping", Map.of()).get("lastvalue"));
            String loss = str(itemByKey.getOrDefault("icmppingloss", Map.of()).get("lastvalue"));
            String lossUnits = str(itemByKey.getOrDefault("icmppingloss", Map.of()).get("units"));
            String sec = str(itemByKey.getOrDefault("icmppingsec", Map.of()).get("lastvalue"));
            String secUnits = str(itemByKey.getOrDefault("icmppingsec", Map.of()).get("units"));

            Integer onlineStatus = null;
            if (StrUtil.isNotBlank(icmpPing)) {
                onlineStatus = "1".equals(icmpPing) ? 1 : 0;
            }
            Integer collectionStatus = (StrUtil.isNotBlank(icmpPing) && StrUtil.isNotBlank(loss) && StrUtil.isNotBlank(sec)) ? 1 : 0;

            NetworkHostEntity existing = existingByIp.get(ip);
            NetworkHostEntity entity = new NetworkHostEntity();
            if (existing != null) {
                entity.setId(existing.getId());
            }
            entity.setInstance(ip);
            entity.setName(hostName);
            entity.setAreaName(resolved.areaName);
            entity.setGroupName(resolved.deviceGroup);
            entity.setDeviceModel(StrUtil.blankToDefault(deviceModel, null));
            entity.setStatus(1);
            entity.setCollectionStatus(collectionStatus);
            entity.setOnlineStatus(onlineStatus);
            entity.setPacketLossRate(formatMetric(loss, lossUnits));
            entity.setResponseTime(formatResponseTime(sec, secUnits));
            entity.setMissingCount(0);
            entity.setLastSeenTime(now);
            entity.setLastSyncTime(now);
            if (existing == null) {
                entity.setCreator(operatorId);
                entity.setUpdater(operatorId);
                networkHostService.save(entity);
                rsp.setInserted(rsp.getInserted() + 1);
            } else {
                entity.setUpdater(operatorId);
                networkHostService.updateById(entity);
                rsp.setUpdated(rsp.getUpdated() + 1);
            }
            syncBackupDevice(entity, backupByHostId.get(entity.getId()), operatorId);
        }

        markMissing(syncedIps, rsp);
        rsp.setUnmatchedAreas(rsp.getUnmatchedAreas().stream().distinct().sorted().toList());
        rsp.setMessage("OK");
        return rsp;
    }

    private void markMissing(Set<String> syncedIps, ZabbixNetworkHostSyncRsp rsp) {
        List<NetworkHostEntity> all = networkHostService.list(new LambdaQueryWrapper<NetworkHostEntity>()
                .select(NetworkHostEntity::getId, NetworkHostEntity::getInstance, NetworkHostEntity::getStatus, NetworkHostEntity::getMissingCount));
        Date now = new Date();
        for (NetworkHostEntity item : all) {
            if (item == null || StrUtil.isBlank(item.getInstance()) || item.getId() == null) {
                continue;
            }
            if (syncedIps.contains(item.getInstance())) {
                continue;
            }
            int missing = Optional.ofNullable(item.getMissingCount()).orElse(0) + 1;
            LambdaUpdateWrapper<NetworkHostEntity> wrapper = new LambdaUpdateWrapper<NetworkHostEntity>()
                    .eq(NetworkHostEntity::getId, item.getId())
                    .set(NetworkHostEntity::getMissingCount, missing)
                    .set(NetworkHostEntity::getLastSyncTime, now);
            if (missing >= MISSING_DISABLE_THRESHOLD) {
                wrapper.set(NetworkHostEntity::getStatus, 0)
                        .set(NetworkHostEntity::getCollectionStatus, 2);
                networkBackupDeviceMapper.update(null, new LambdaUpdateWrapper<NetworkBackupDeviceEntity>()
                        .eq(NetworkBackupDeviceEntity::getNetworkHostId, item.getId())
                        .set(NetworkBackupDeviceEntity::getStatus, 0)
                        .set(NetworkBackupDeviceEntity::getBackupEnabled, 0));
                if (!Integer.valueOf(0).equals(item.getStatus())) {
                    rsp.setDisabled(rsp.getDisabled() + 1);
                    rsp.setLogicalDeleted(rsp.getLogicalDeleted() + 1);
                }
            }
            networkHostService.update(wrapper);
        }
    }

    private void syncBackupDevice(NetworkHostEntity host, NetworkBackupDeviceEntity backup, Long operatorId) {
        if (host == null || host.getId() == null || StrUtil.isBlank(host.getInstance())) {
            return;
        }
        Date now = new Date();
        if (backup == null) {
            NetworkBackupDeviceEntity entity = new NetworkBackupDeviceEntity();
            entity.setNetworkHostId(host.getId());
            entity.setInstance(host.getInstance());
            entity.setName(host.getName());
            entity.setAreaName(host.getAreaName());
            entity.setGroupName(host.getGroupName());
            entity.setDeviceModel(host.getDeviceModel());
            entity.setStatus(host.getStatus());
            entity.setBackupEnabled(0);
            entity.setCreator(operatorId);
            entity.setUpdater(operatorId);
            entity.setCreateDate(now);
            entity.setUpdateDate(now);
            networkBackupDeviceMapper.insert(entity);
            return;
        }
        NetworkBackupDeviceEntity entity = new NetworkBackupDeviceEntity();
        entity.setId(backup.getId());
        entity.setNetworkHostId(host.getId());
        entity.setInstance(host.getInstance());
        entity.setName(host.getName());
        entity.setAreaName(host.getAreaName());
        entity.setGroupName(host.getGroupName());
        entity.setDeviceModel(host.getDeviceModel());
        entity.setStatus(host.getStatus());
        entity.setUpdater(operatorId);
        networkBackupDeviceMapper.updateById(entity);
    }

    private HostGroupResolved resolveHostGroup(Object hostgroupsObj,
                                               Map<String, ZabbixNetworkDeviceMappingPreviewRsp.GroupPreviewItem> groupRuleMap) {
        if (!(hostgroupsObj instanceof List<?> groups) || groups.isEmpty()) {
            return null;
        }
        for (Object g : groups) {
            if (!(g instanceof Map<?, ?> map)) {
                continue;
            }
            String gid = str(map.get("groupid"));
            if (StrUtil.isBlank(gid)) {
                continue;
            }
            var preview = groupRuleMap.get(gid);
            if (preview == null) {
                continue;
            }
            HostGroupResolved resolved = new HostGroupResolved();
            resolved.areaName = preview.getMatchedAreaName();
            resolved.deviceGroup = preview.getMappedDeviceGroup();
            resolved.rawGroupName = preview.getGroupName();
            return resolved;
        }
        return null;
    }

    private String resolveDeviceModel(Object parentTemplatesObj, Map<String, String> templateModelMap) {
        if (!(parentTemplatesObj instanceof List<?> templates)) {
            return null;
        }
        for (Object t : templates) {
            if (!(t instanceof Map<?, ?> map)) {
                continue;
            }
            String name = str(map.get("name"));
            if (StrUtil.isBlank(name)) {
                continue;
            }
            String mapped = templateModelMap.get(name);
            if (StrUtil.isNotBlank(mapped)) {
                return mapped;
            }
        }
        return null;
    }

    private String extractHostIp(Object interfacesObj) {
        if (!(interfacesObj instanceof List<?> list)) {
            return null;
        }
        for (Object item : list) {
            if (!(item instanceof Map<?, ?> map)) {
                continue;
            }
            String ip = str(map.get("ip"));
            if (StrUtil.isNotBlank(ip)) {
                return ip;
            }
        }
        return null;
    }

    private String formatMetric(String val, String units) {
        if (StrUtil.isBlank(val)) {
            return null;
        }
        return StrUtil.isBlank(units) ? val : (val + units);
    }

    private String formatResponseTime(String val, String units) {
        if (StrUtil.isBlank(val)) {
            return null;
        }
        try {
            if ("s".equalsIgnoreCase(StrUtil.blankToDefault(units, ""))) {
                double sec = Double.parseDouble(val);
                return String.format(java.util.Locale.ROOT, "%.2fms", sec * 1000);
            }
        } catch (Exception ignored) {
        }
        return formatMetric(val, units);
    }

    private String str(Object value) {
        return value == null ? null : String.valueOf(value);
    }

    private static class HostGroupResolved {
        private String areaName;
        private String deviceGroup;
        @SuppressWarnings("unused")
        private String rawGroupName;
    }
}
