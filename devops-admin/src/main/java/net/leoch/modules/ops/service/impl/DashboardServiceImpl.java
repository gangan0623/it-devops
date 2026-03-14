package net.leoch.modules.ops.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.modules.alert.service.AlertRealtimeViewService;
import net.leoch.modules.alert.vo.rsp.AlertRealtimeRsp;
import net.leoch.modules.ops.entity.BusinessSystemEntity;
import net.leoch.modules.ops.entity.LinuxHostEntity;
import net.leoch.modules.ops.entity.MonitorComponentEntity;
import net.leoch.modules.ops.entity.NetworkHostEntity;
import net.leoch.modules.ops.entity.WindowHostEntity;
import net.leoch.modules.ops.mapper.BusinessSystemMapper;
import net.leoch.modules.ops.mapper.NetworkDeviceBackupRecordMapper;
import net.leoch.modules.ops.mapper.LinuxHostMapper;
import net.leoch.modules.ops.mapper.MonitorComponentMapper;
import net.leoch.modules.ops.mapper.NetworkHostMapper;
import net.leoch.modules.ops.mapper.WindowHostMapper;
import net.leoch.modules.ops.service.IDashboardService;
import net.leoch.modules.ops.vo.rsp.*;
import org.springframework.stereotype.Service;

import java.util.*;

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
    private final NetworkHostMapper networkHostMapper;
    private final NetworkDeviceBackupRecordMapper deviceBackupRecordMapper;
    private final MonitorComponentMapper monitorComponentMapper;
    private final AlertRealtimeViewService alertRealtimeViewService;

    @Override
    public DashboardSummaryRsp summary() {
        DashboardSummaryRsp data = new DashboardSummaryRsp();
        data.setHostCounts(buildHostCounts());
        data.setBackupStats(buildBackupStats());
        data.setRecentAlerts(buildRecentAlerts());
        data.setMonitorComponents(buildMonitorComponents());
        return data;
    }

    private DashboardHostCountsRsp buildHostCounts() {
        DashboardHostCountsRsp hostCounts = new DashboardHostCountsRsp();

        List<WindowHostEntity> windows = windowHostMapper.selectList(
                new LambdaQueryWrapper<WindowHostEntity>()
                        .select(WindowHostEntity::getType, WindowHostEntity::getOnlineStatus)
        );
        List<LinuxHostEntity> linuxes = linuxHostMapper.selectList(
                new LambdaQueryWrapper<LinuxHostEntity>()
                        .select(LinuxHostEntity::getType, LinuxHostEntity::getOnlineStatus)
        );
        List<BusinessSystemEntity> businesses = businessSystemMapper.selectList(
                new LambdaQueryWrapper<BusinessSystemEntity>()
                        .select(BusinessSystemEntity::getOnlineStatus)
        );
        List<NetworkHostEntity> networks = networkHostMapper.selectList(
                new LambdaQueryWrapper<NetworkHostEntity>()
                        .select(NetworkHostEntity::getOnlineStatus)
        );

        long windowsOnline = 0L;
        long windowsOffline = 0L;
        long linuxOnline = 0L;
        long linuxOffline = 0L;
        long businessOnline = 0L;
        long businessOffline = 0L;
        long networkOnline = 0L;
        long networkOffline = 0L;
        long physicalOnline = 0L;
        long physicalOffline = 0L;
        long vmOnline = 0L;
        long vmOffline = 0L;

        for (WindowHostEntity item : windows) {
            if (Boolean.TRUE.equals(item.getOnlineStatus())) {
                windowsOnline++;
            } else {
                windowsOffline++;
            }
            if ("Physical".equalsIgnoreCase(StrUtil.blankToDefault(item.getType(), ""))) {
                if (Boolean.TRUE.equals(item.getOnlineStatus())) {
                    physicalOnline++;
                } else {
                    physicalOffline++;
                }
            } else if ("VM".equalsIgnoreCase(StrUtil.blankToDefault(item.getType(), ""))) {
                if (Boolean.TRUE.equals(item.getOnlineStatus())) {
                    vmOnline++;
                } else {
                    vmOffline++;
                }
            }
        }

        for (LinuxHostEntity item : linuxes) {
            if (Boolean.TRUE.equals(item.getOnlineStatus())) {
                linuxOnline++;
            } else {
                linuxOffline++;
            }
            if ("Physical".equalsIgnoreCase(StrUtil.blankToDefault(item.getType(), ""))) {
                if (Boolean.TRUE.equals(item.getOnlineStatus())) {
                    physicalOnline++;
                } else {
                    physicalOffline++;
                }
            } else if ("VM".equalsIgnoreCase(StrUtil.blankToDefault(item.getType(), ""))) {
                if (Boolean.TRUE.equals(item.getOnlineStatus())) {
                    vmOnline++;
                } else {
                    vmOffline++;
                }
            }
        }

        for (BusinessSystemEntity item : businesses) {
            if (Boolean.TRUE.equals(item.getOnlineStatus())) {
                businessOnline++;
            } else {
                businessOffline++;
            }
        }
        for (NetworkHostEntity item : networks) {
            if (item != null && Integer.valueOf(1).equals(item.getOnlineStatus())) {
                networkOnline++;
            } else {
                networkOffline++;
            }
        }

        hostCounts.setWindows((long) windows.size());
        hostCounts.setWindowsOnline(windowsOnline);
        hostCounts.setWindowsOffline(windowsOffline);

        hostCounts.setLinux((long) linuxes.size());
        hostCounts.setLinuxOnline(linuxOnline);
        hostCounts.setLinuxOffline(linuxOffline);

        hostCounts.setBusiness((long) businesses.size());
        hostCounts.setBusinessOnline(businessOnline);
        hostCounts.setBusinessOffline(businessOffline);

        hostCounts.setNetwork((long) networks.size());
        hostCounts.setNetworkOnline(networkOnline);
        hostCounts.setNetworkOffline(networkOffline);
        hostCounts.setAssetTotal((long) windows.size() + linuxes.size() + businesses.size() + networks.size());

        hostCounts.setPhysical(physicalOnline + physicalOffline);
        hostCounts.setPhysicalOnline(physicalOnline);
        hostCounts.setPhysicalOffline(physicalOffline);

        hostCounts.setVm(vmOnline + vmOffline);
        hostCounts.setVmOnline(vmOnline);
        hostCounts.setVmOffline(vmOffline);
        return hostCounts;
    }

    private DashboardBackupStatsRsp buildBackupStats() {
        // 使用聚合查询一次性获取所有统计信息，替代5次独立查询
        DashboardBackupStatsRsp backupStats = deviceBackupRecordMapper.getBackupStats();
        log.debug("[看板] 备份统计, total={}, success={}, fail={}, round={}",
                backupStats.getTotal(), backupStats.getSuccess(), backupStats.getFail(), backupStats.getRound());
        return backupStats;
    }

    private List<AlertRealtimeRsp> buildRecentAlerts() {
        return alertRealtimeViewService.recentAlerts(10);
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

}
