package net.leoch.modules.ops.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import net.leoch.modules.alert.mapper.AlertRecordMapper;
import net.leoch.modules.alert.entity.AlertRecordEntity;
import net.leoch.modules.ops.mapper.*;
import net.leoch.modules.ops.dto.*;
import net.leoch.modules.ops.entity.BusinessSystemEntity;
import net.leoch.modules.ops.entity.DeviceBackupEntity;
import net.leoch.modules.ops.entity.DeviceBackupRecordEntity;
import net.leoch.modules.ops.entity.LinuxHostEntity;
import net.leoch.modules.ops.entity.MonitorComponentEntity;
import net.leoch.modules.ops.entity.WindowHostEntity;
import net.leoch.modules.ops.service.DashboardService;
import net.leoch.modules.ops.service.ZabbixClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * 工作台统计
 */
@Slf4j
@Service
public class DashboardServiceImpl implements DashboardService {

    private final WindowHostDao windowHostDao;
    private final LinuxHostDao linuxHostDao;
    private final BusinessSystemDao businessSystemDao;
    private final DeviceBackupDao deviceBackupDao;
    private final DeviceBackupRecordDao deviceBackupRecordDao;
    private final MonitorComponentDao monitorComponentDao;
    private final AlertRecordDao alertRecordDao;
    private final ZabbixClient zabbixClient;

    public DashboardServiceImpl(WindowHostDao windowHostDao,
                                LinuxHostDao linuxHostDao,
                                BusinessSystemDao businessSystemDao,
                                DeviceBackupDao deviceBackupDao,
                                DeviceBackupRecordDao deviceBackupRecordDao,
                                MonitorComponentDao monitorComponentDao,
                                AlertRecordDao alertRecordDao,
                                ZabbixClient zabbixClient) {
        this.windowHostDao = windowHostDao;
        this.linuxHostDao = linuxHostDao;
        this.businessSystemDao = businessSystemDao;
        this.deviceBackupDao = deviceBackupDao;
        this.deviceBackupRecordDao = deviceBackupRecordDao;
        this.monitorComponentDao = monitorComponentDao;
        this.alertRecordDao = alertRecordDao;
        this.zabbixClient = zabbixClient;
    }

    @Override
    public DashboardSummaryResponse summary() {
        DashboardSummaryResponse data = new DashboardSummaryResponse();

        DashboardHostCounts hostCounts = new DashboardHostCounts();
        hostCounts.setWindows(windowHostDao.selectCount(new LambdaQueryWrapper<>()));
        hostCounts.setLinux(linuxHostDao.selectCount(new LambdaQueryWrapper<>()));
        hostCounts.setBusiness(businessSystemDao.selectCount(new LambdaQueryWrapper<>()));
        data.setHostCounts(hostCounts);

        DashboardBackupStats backupStats = new DashboardBackupStats();
        long total = deviceBackupRecordDao.selectCount(new LambdaQueryWrapper<>());
        long success = deviceBackupRecordDao.selectCount(
                new LambdaQueryWrapper<DeviceBackupRecordEntity>().eq(DeviceBackupRecordEntity::getLastBackupStatus, 1)
        );
        long fail = deviceBackupRecordDao.selectCount(
                new LambdaQueryWrapper<DeviceBackupRecordEntity>().eq(DeviceBackupRecordEntity::getLastBackupStatus, 0)
        );
        DeviceBackupRecordEntity maxBackup = deviceBackupRecordDao.selectOne(
                new LambdaQueryWrapper<DeviceBackupRecordEntity>()
                        .select(DeviceBackupRecordEntity::getBackupNum)
                        .orderByDesc(DeviceBackupRecordEntity::getBackupNum)
                        .last("limit 1")
        );
        Integer round = maxBackup == null ? 0 : maxBackup.getBackupNum();
        DeviceBackupRecordEntity lastBackup = deviceBackupRecordDao.selectOne(
                new LambdaQueryWrapper<DeviceBackupRecordEntity>()
                        .select(DeviceBackupRecordEntity::getLastBackupTime)
                        .orderByDesc(DeviceBackupRecordEntity::getLastBackupTime)
                        .last("limit 1")
        );
        backupStats.setRound(round == null ? 0 : round);
        backupStats.setTotal(total);
        backupStats.setSuccess(success);
        backupStats.setFail(fail);
        backupStats.setLastTime(lastBackup == null ? null : lastBackup.getLastBackupTime());
        data.setBackupStats(backupStats);

        List<Map<String, String>> zabbixHosts = zabbixClient.getHostsByTemplates();
        List<DeviceBackupEntity> backupDevices = deviceBackupDao.selectList(
                new LambdaQueryWrapper<DeviceBackupEntity>()
                        .select(DeviceBackupEntity::getInstance, DeviceBackupEntity::getName)
        );
        Set<String> zabbixIps = new HashSet<>();
        for (Map<String, String> host : zabbixHosts) {
            String ip = host.get("ip");
            if (StrUtil.isNotBlank(ip)) {
                zabbixIps.add(ip);
            }
        }
        Set<String> backupIps = new HashSet<>();
        for (DeviceBackupEntity device : backupDevices) {
            if (device != null && StrUtil.isNotBlank(device.getInstance())) {
                backupIps.add(device.getInstance());
            }
        }
        List<DashboardDeviceDiffItem> zabbixOnly = new ArrayList<>();
        for (Map<String, String> host : zabbixHosts) {
            String ip = host.get("ip");
            if (StrUtil.isBlank(ip) || backupIps.contains(ip)) {
                continue;
            }
            DashboardDeviceDiffItem item = new DashboardDeviceDiffItem();
            item.setIp(ip);
            item.setName(host.get("name"));
            zabbixOnly.add(item);
        }
        List<DashboardDeviceDiffItem> backupOnly = new ArrayList<>();
        for (DeviceBackupEntity device : backupDevices) {
            if (device == null || StrUtil.isBlank(device.getInstance())) {
                continue;
            }
            if (zabbixIps.contains(device.getInstance())) {
                continue;
            }
            DashboardDeviceDiffItem item = new DashboardDeviceDiffItem();
            item.setIp(device.getInstance());
            item.setName(device.getName());
            backupOnly.add(item);
        }
        DashboardDeviceDiff diff = new DashboardDeviceDiff();
        diff.setZabbixOnly(zabbixOnly);
        diff.setBackupOnly(backupOnly);
        data.setDeviceDiff(diff);

        List<AlertRecordEntity> alerts = alertRecordDao.selectList(
                new LambdaQueryWrapper<AlertRecordEntity>()
                        .select(AlertRecordEntity::getAlertName, AlertRecordEntity::getInstance, AlertRecordEntity::getStartsAt,
                                AlertRecordEntity::getSeverity, AlertRecordEntity::getStatus)
                        .orderByDesc(AlertRecordEntity::getStartsAt)
                        .last("limit 10")
        );
        Map<String, String> hostMap = loadHostMap();
        List<DashboardAlertSummary> recentAlerts = new ArrayList<>();
        for (AlertRecordEntity alert : alerts) {
            DashboardAlertSummary item = new DashboardAlertSummary();
            item.setAlertName(alert.getAlertName());
            item.setInstance(alert.getInstance());
            item.setHostName(hostMap.get(normalizeInstance(alert.getInstance())));
            item.setTime(alert.getStartsAt());
            item.setSeverity(alert.getSeverity());
            item.setStatus(alert.getStatus());
            recentAlerts.add(item);
        }
        data.setRecentAlerts(recentAlerts);

        List<MonitorComponentEntity> components = monitorComponentDao.selectList(
                new LambdaQueryWrapper<MonitorComponentEntity>()
                        .select(MonitorComponentEntity::getName, MonitorComponentEntity::getOnlineStatus, MonitorComponentEntity::getUpdateAvailable)
                        .orderByDesc(MonitorComponentEntity::getUpdateDate)
        );
        List<DashboardMonitorComponentItem> monitorItems = new ArrayList<>();
        for (MonitorComponentEntity component : components) {
            DashboardMonitorComponentItem item = new DashboardMonitorComponentItem();
            item.setName(component.getName());
            item.setOnlineStatus(component.getOnlineStatus());
            item.setUpdateAvailable(component.getUpdateAvailable());
            monitorItems.add(item);
        }
        data.setMonitorComponents(monitorItems);

        return data;
    }

    private Map<String, String> loadHostMap() {
        Map<String, String> map = new HashMap<>();
        List<LinuxHostEntity> linuxList = linuxHostDao.selectList(new LambdaQueryWrapper<LinuxHostEntity>().select(LinuxHostEntity::getInstance, LinuxHostEntity::getName));
        for (LinuxHostEntity item : linuxList) {
            putHost(map, item.getInstance(), item.getName());
        }
        List<WindowHostEntity> winList = windowHostDao.selectList(new LambdaQueryWrapper<WindowHostEntity>().select(WindowHostEntity::getInstance, WindowHostEntity::getName));
        for (WindowHostEntity item : winList) {
            putHost(map, item.getInstance(), item.getName());
        }
        List<BusinessSystemEntity> businessList = businessSystemDao.selectList(new LambdaQueryWrapper<BusinessSystemEntity>().select(BusinessSystemEntity::getInstance, BusinessSystemEntity::getName));
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
