package net.leoch.modules.ops.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.modules.alert.entity.AlertRecordEntity;
import net.leoch.modules.alert.mapper.AlertRecordMapper;
import net.leoch.modules.ops.entity.*;
import net.leoch.modules.ops.mapper.*;
import net.leoch.modules.ops.service.IDashboardService;
import net.leoch.modules.ops.service.ZabbixClient;
import net.leoch.modules.ops.vo.rsp.*;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 工作台统计
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class DashboardServiceImpl implements IDashboardService {

    private final WindowHostMapper windowHostMapper;
    private final LinuxHostMapper linuxHostMapper;
    private final BusinessSystemMapper businessSystemMapper;
    private final DeviceBackupMapper deviceBackupMapper;
    private final DeviceBackupRecordMapper deviceBackupRecordMapper;
    private final MonitorComponentMapper monitorComponentMapper;
    private final AlertRecordMapper alertRecordMapper;
    private final ZabbixClient zabbixClient;

    @Override
    public DashboardSummaryRsp summary() {
        DashboardSummaryRsp data = new DashboardSummaryRsp();
        data.setHostCounts(buildHostCounts());
        data.setBackupStats(buildBackupStats());
        data.setDeviceDiff(buildDeviceDiff());
        data.setRecentAlerts(buildRecentAlerts());
        data.setMonitorComponents(buildMonitorComponents());
        return data;
    }

    private DashboardHostCountsRsp buildHostCounts() {
        DashboardHostCountsRsp hostCounts = new DashboardHostCountsRsp();
        hostCounts.setWindows(windowHostMapper.selectCount(new LambdaQueryWrapper<>()));
        hostCounts.setLinux(linuxHostMapper.selectCount(new LambdaQueryWrapper<>()));
        hostCounts.setBusiness(businessSystemMapper.selectCount(new LambdaQueryWrapper<>()));
        return hostCounts;
    }

    private DashboardBackupStatsRsp buildBackupStats() {
        // 使用聚合查询一次性获取所有统计信息，替代5次独立查询
        DashboardBackupStatsRsp backupStats = deviceBackupRecordMapper.getBackupStats();
        log.debug("[看板] 备份统计, total={}, success={}, fail={}, round={}",
                backupStats.getTotal(), backupStats.getSuccess(), backupStats.getFail(), backupStats.getRound());
        return backupStats;
    }

    private DashboardDeviceDiffRsp buildDeviceDiff() {
        List<Map<String, String>> zabbixHosts = zabbixClient.getHostsByTemplates();
        List<DeviceBackupEntity> backupDevices = deviceBackupMapper.selectList(
                new LambdaQueryWrapper<DeviceBackupEntity>()
                        .select(DeviceBackupEntity::getInstance, DeviceBackupEntity::getName)
        );

        // 使用 Stream API 构建 IP 集合，提升代码可读性
        Set<String> zabbixIps = zabbixHosts.stream()
                .map(host -> host.get("ip"))
                .filter(StrUtil::isNotBlank)
                .collect(Collectors.toSet());

        Set<String> backupIps = backupDevices.stream()
                .filter(device -> device != null && StrUtil.isNotBlank(device.getInstance()))
                .map(DeviceBackupEntity::getInstance)
                .collect(Collectors.toSet());

        // 找出只在 Zabbix 中的设备（差集运算 O(n)）
        List<DashboardDeviceDiffItemRsp> zabbixOnly = zabbixHosts.stream()
                .filter(host -> StrUtil.isNotBlank(host.get("ip")) && !backupIps.contains(host.get("ip")))
                .map(host -> {
                    DashboardDeviceDiffItemRsp item = new DashboardDeviceDiffItemRsp();
                    item.setIp(host.get("ip"));
                    item.setName(host.get("name"));
                    return item;
                })
                .collect(Collectors.toList());

        // 找出只在备份中的设备（差集运算 O(n)）
        List<DashboardDeviceDiffItemRsp> backupOnly = backupDevices.stream()
                .filter(device -> device != null && StrUtil.isNotBlank(device.getInstance()))
                .filter(device -> !zabbixIps.contains(device.getInstance()))
                .map(device -> {
                    DashboardDeviceDiffItemRsp item = new DashboardDeviceDiffItemRsp();
                    item.setIp(device.getInstance());
                    item.setName(device.getName());
                    return item;
                })
                .collect(Collectors.toList());

        log.debug("[看板] 设备差异分析, zabbix={}, backup={}, zabbixOnly={}, backupOnly={}",
                zabbixHosts.size(), backupDevices.size(), zabbixOnly.size(), backupOnly.size());

        DashboardDeviceDiffRsp diff = new DashboardDeviceDiffRsp();
        diff.setZabbixOnly(zabbixOnly);
        diff.setBackupOnly(backupOnly);
        return diff;
    }

    private List<DashboardAlertSummaryRsp> buildRecentAlerts() {
        List<AlertRecordEntity> alerts = alertRecordMapper.selectList(
                new LambdaQueryWrapper<AlertRecordEntity>()
                        .select(AlertRecordEntity::getAlertName, AlertRecordEntity::getInstance, AlertRecordEntity::getCreateDate,
                                AlertRecordEntity::getSeverity, AlertRecordEntity::getStatus)
                        .orderByDesc(AlertRecordEntity::getCreateDate)
                        .last("limit 10")
        );
        Map<String, String> hostMap = loadHostMap();
        List<DashboardAlertSummaryRsp> recentAlerts = new ArrayList<>();
        for (AlertRecordEntity alert : alerts) {
            DashboardAlertSummaryRsp item = new DashboardAlertSummaryRsp();
            item.setAlertName(alert.getAlertName());
            item.setInstance(alert.getInstance());
            item.setHostName(hostMap.get(normalizeInstance(alert.getInstance())));
            item.setTime(alert.getCreateDate());
            item.setSeverity(alert.getSeverity());
            item.setStatus(alert.getStatus());
            recentAlerts.add(item);
        }
        return recentAlerts;
    }

    private List<DashboardMonitorComponentItemRsp> buildMonitorComponents() {
        List<MonitorComponentEntity> components = monitorComponentMapper.selectList(
                new LambdaQueryWrapper<MonitorComponentEntity>()
                        .select(MonitorComponentEntity::getName, MonitorComponentEntity::getOnlineStatus, MonitorComponentEntity::getUpdateAvailable)
                        .orderByDesc(MonitorComponentEntity::getUpdateDate)
        );
        List<DashboardMonitorComponentItemRsp> monitorItems = new ArrayList<>();
        for (MonitorComponentEntity component : components) {
            DashboardMonitorComponentItemRsp item = new DashboardMonitorComponentItemRsp();
            item.setName(component.getName());
            item.setOnlineStatus(component.getOnlineStatus());
            item.setUpdateAvailable(component.getUpdateAvailable());
            monitorItems.add(item);
        }
        return monitorItems;
    }

    private Map<String, String> loadHostMap() {
        Map<String, String> map = new HashMap<>();
        List<LinuxHostEntity> linuxList = linuxHostMapper.selectList(new LambdaQueryWrapper<LinuxHostEntity>().select(LinuxHostEntity::getInstance, LinuxHostEntity::getName));
        for (LinuxHostEntity item : linuxList) {
            putHost(map, item.getInstance(), item.getName());
        }
        List<WindowHostEntity> winList = windowHostMapper.selectList(new LambdaQueryWrapper<WindowHostEntity>().select(WindowHostEntity::getInstance, WindowHostEntity::getName));
        for (WindowHostEntity item : winList) {
            putHost(map, item.getInstance(), item.getName());
        }
        List<BusinessSystemEntity> businessList = businessSystemMapper.selectList(new LambdaQueryWrapper<BusinessSystemEntity>().select(BusinessSystemEntity::getInstance, BusinessSystemEntity::getName));
        for (BusinessSystemEntity item : businessList) {
            putHost(map, item.getInstance(), item.getName());
        }
        return map;
    }

    private void putHost(Map<String, String> map, String instance, String name) {
        String key = normalizeInstance(instance);
        if (StrUtil.isBlank(key)) {
            return;
        }
        map.put(key, StrUtil.blankToDefault(name, key));
    }

    private String normalizeInstance(String instance) {
        if (StrUtil.isBlank(instance)) {
            return null;
        }
        String value = instance.trim();
        if (value.startsWith("http://")) {
            value = value.substring(7);
        } else if (value.startsWith("https://")) {
            value = value.substring(8);
        }
        int slash = value.indexOf('/');
        if (slash > -1) {
            value = value.substring(0, slash);
        }
        int colon = value.indexOf(':');
        if (colon > -1) {
            value = value.substring(0, colon);
        }
        return value.trim();
    }
}
