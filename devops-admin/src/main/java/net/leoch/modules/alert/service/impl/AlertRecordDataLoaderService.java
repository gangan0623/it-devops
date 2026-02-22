package net.leoch.modules.alert.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.modules.alert.entity.AlertRecordEntity;
import net.leoch.modules.ops.entity.BusinessSystemEntity;
import net.leoch.modules.ops.entity.LinuxHostEntity;
import net.leoch.modules.ops.entity.WindowHostEntity;
import net.leoch.modules.ops.mapper.BusinessSystemMapper;
import net.leoch.modules.ops.mapper.LinuxHostMapper;
import net.leoch.modules.ops.mapper.WindowHostMapper;
import net.leoch.modules.sys.entity.SysUserEntity;
import net.leoch.modules.sys.mapper.SysUserMapper;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * 告警记录数据加载服务
 * 职责：加载主机信息、用户信息等辅助数据
 *
 * @author Claude Sonnet 4.5
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class AlertRecordDataLoaderService {

    private final LinuxHostMapper linuxHostMapper;
    private final WindowHostMapper windowHostMapper;
    private final BusinessSystemMapper businessSystemMapper;
    private final SysUserMapper sysUserMapper;

    /**
     * 加载主机信息映射表
     * @return 实例 -> 主机信息的映射
     */
    public Map<String, HostInfo> loadHostInfoMap() {
        Map<String, HostInfo> map = new HashMap<>();

        // 只查询必要字段，添加上限保护
        List<LinuxHostEntity> linuxList = linuxHostMapper.selectList(
            new LambdaQueryWrapper<LinuxHostEntity>()
                .select(LinuxHostEntity::getInstance, LinuxHostEntity::getName)
                .last("LIMIT 10000")
        );
        for (LinuxHostEntity item : linuxList) {
            putHost(map, item.getInstance(), item.getName(), "linux");
        }

        List<WindowHostEntity> winList = windowHostMapper.selectList(
            new LambdaQueryWrapper<WindowHostEntity>()
                .select(WindowHostEntity::getInstance, WindowHostEntity::getName)
                .last("LIMIT 10000")
        );
        for (WindowHostEntity item : winList) {
            putHost(map, item.getInstance(), item.getName(), "windows");
        }

        List<BusinessSystemEntity> businessList = businessSystemMapper.selectList(
            new LambdaQueryWrapper<BusinessSystemEntity>()
                .select(BusinessSystemEntity::getInstance, BusinessSystemEntity::getName)
                .last("LIMIT 10000")
        );
        for (BusinessSystemEntity item : businessList) {
            putHost(map, item.getInstance(), item.getName(), "business");
        }

        log.debug("[告警记录数据加载] 加载主机映射, linux={}, windows={}, business={}",
            linuxList.size(), winList.size(), businessList.size());
        return map;
    }

    /**
     * 加载用户映射表
     * @param userIds 用户 ID 列表
     * @return 用户 ID -> 用户名的映射
     */
    public Map<Long, String> loadUserMap(List<Long> userIds) {
        if (userIds == null || userIds.isEmpty()) {
            return new HashMap<>();
        }
        List<SysUserEntity> users = sysUserMapper.selectList(
            new LambdaQueryWrapper<SysUserEntity>()
                .select(SysUserEntity::getId, SysUserEntity::getUsername)
                .in(SysUserEntity::getId, userIds)
        );
        return users.stream()
            .collect(Collectors.toMap(SysUserEntity::getId, SysUserEntity::getUsername, (a, b) -> a));
    }

    /**
     * 根据主机名关键字查询实例列表
     * @param keyword 关键字
     * @param deviceType 设备类型
     * @return 实例列表
     */
    public List<String> listInstancesByHostName(String keyword, String deviceType) {
        if (StrUtil.isBlank(keyword)) {
            return new ArrayList<>();
        }
        String value = keyword.trim();
        List<String> instances = new ArrayList<>();

        // 使用函数式接口定义主机查询策略
        if (StrUtil.isBlank(deviceType) || "linux".equalsIgnoreCase(deviceType)) {
            instances.addAll(queryHostInstances(
                () -> linuxHostMapper.selectList(
                    new LambdaQueryWrapper<LinuxHostEntity>()
                        .select(LinuxHostEntity::getInstance)
                        .like(LinuxHostEntity::getName, value)
                ),
                LinuxHostEntity::getInstance
            ));
        }
        if (StrUtil.isBlank(deviceType) || "windows".equalsIgnoreCase(deviceType)) {
            instances.addAll(queryHostInstances(
                () -> windowHostMapper.selectList(
                    new LambdaQueryWrapper<WindowHostEntity>()
                        .select(WindowHostEntity::getInstance)
                        .like(WindowHostEntity::getName, value)
                ),
                WindowHostEntity::getInstance
            ));
        }
        if (StrUtil.isBlank(deviceType) || "business".equalsIgnoreCase(deviceType)) {
            instances.addAll(queryHostInstances(
                () -> businessSystemMapper.selectList(
                    new LambdaQueryWrapper<BusinessSystemEntity>()
                        .select(BusinessSystemEntity::getInstance)
                        .like(BusinessSystemEntity::getName, value)
                ),
                BusinessSystemEntity::getInstance
            ));
        }

        return instances.stream().filter(StrUtil::isNotBlank).distinct().collect(Collectors.toList());
    }

    /**
     * 按设备类型列出实例
     * @param deviceType 设备类型
     * @return 实例列表
     */
    public List<String> listInstancesByType(String deviceType) {
        List<String> instances = new ArrayList<>();
        if (StrUtil.isBlank(deviceType) || "linux".equalsIgnoreCase(deviceType)) {
            instances.addAll(queryHostInstances(
                () -> linuxHostMapper.selectList(
                    new LambdaQueryWrapper<LinuxHostEntity>()
                        .select(LinuxHostEntity::getInstance)
                ),
                LinuxHostEntity::getInstance
            ));
        }
        if (StrUtil.isBlank(deviceType) || "windows".equalsIgnoreCase(deviceType)) {
            instances.addAll(queryHostInstances(
                () -> windowHostMapper.selectList(
                    new LambdaQueryWrapper<WindowHostEntity>()
                        .select(WindowHostEntity::getInstance)
                ),
                WindowHostEntity::getInstance
            ));
        }
        if (StrUtil.isBlank(deviceType) || "business".equalsIgnoreCase(deviceType)) {
            instances.addAll(queryHostInstances(
                () -> businessSystemMapper.selectList(
                    new LambdaQueryWrapper<BusinessSystemEntity>()
                        .select(BusinessSystemEntity::getInstance)
                ),
                BusinessSystemEntity::getInstance
            ));
        }
        return instances.stream().filter(StrUtil::isNotBlank).distinct().collect(Collectors.toList());
    }

    /**
     * 规范化实例名称
     * @param instance 原始实例名
     * @return 规范化后的实例名
     */
    public String normalizeInstance(String instance) {
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

    /**
     * 解析主机名
     * @param instance 实例
     * @param hostMap 主机映射
     * @return 主机名
     */
    public String resolveHostName(String instance, Map<String, HostInfo> hostMap) {
        if (StrUtil.isBlank(instance) || hostMap == null) {
            return instance;
        }
        String key = normalizeInstance(instance);
        HostInfo info = hostMap.get(key);
        return info == null ? instance : info.getName();
    }

    // ==================== 私有辅助方法 ====================

    private void putHost(Map<String, HostInfo> map, String instance, String name, String type) {
        String key = normalizeInstance(instance);
        if (StrUtil.isBlank(key)) {
            return;
        }
        map.put(key, new HostInfo(StrUtil.blankToDefault(name, key), type));
    }

    private <T> List<String> queryHostInstances(
        java.util.function.Supplier<List<T>> querySupplier,
        Function<T, String> instanceExtractor
    ) {
        return querySupplier.get().stream()
            .map(instanceExtractor)
            .filter(StrUtil::isNotBlank)
            .collect(Collectors.toList());
    }

    /**
     * 主机信息内部类
     */
    public static class HostInfo {
        private final String name;
        private final String type;

        public HostInfo(String name, String type) {
            this.name = name;
            this.type = type;
        }

        public String getName() {
            return name;
        }

        public String getType() {
            return type;
        }
    }
}
