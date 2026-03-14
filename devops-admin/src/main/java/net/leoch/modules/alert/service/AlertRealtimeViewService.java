package net.leoch.modules.alert.service;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.RequiredArgsConstructor;
import net.leoch.modules.alert.entity.AlertRecordEntity;
import net.leoch.modules.alert.mapper.AlertRecordMapper;
import net.leoch.modules.alert.vo.rsp.AlertRealtimeRsp;
import net.leoch.modules.ops.entity.BusinessSystemEntity;
import net.leoch.modules.ops.entity.LinuxHostEntity;
import net.leoch.modules.ops.entity.WindowHostEntity;
import net.leoch.modules.ops.mapper.BusinessSystemMapper;
import net.leoch.modules.ops.mapper.LinuxHostMapper;
import net.leoch.modules.ops.mapper.WindowHostMapper;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
@RequiredArgsConstructor
public class AlertRealtimeViewService {
    private static final long HOME_REALTIME_WINDOW_HOURS = 24L;

    private final AlertRecordMapper alertRecordMapper;
    private final LinuxHostMapper linuxHostMapper;
    private final WindowHostMapper windowHostMapper;
    private final BusinessSystemMapper businessSystemMapper;

    public List<AlertRealtimeRsp> recentAlerts(int limit) {
        int size = limit <= 0 ? 10 : Math.min(limit, 50);
        List<AlertRecordEntity> list = alertRecordMapper.selectList(
                new LambdaQueryWrapper<AlertRecordEntity>()
                        .select(AlertRecordEntity::getAlertName, AlertRecordEntity::getInstance,
                                AlertRecordEntity::getSeverity, AlertRecordEntity::getStatus,
                                AlertRecordEntity::getCreateDate, AlertRecordEntity::getStartsAt,
                                AlertRecordEntity::getEndsAt, AlertRecordEntity::getSummary,
                                AlertRecordEntity::getDescription, AlertRecordEntity::getClosed)
                        .ge(AlertRecordEntity::getStartsAt, java.util.Date.from(Instant.now().minusSeconds(HOME_REALTIME_WINDOW_HOURS * 3600)))
                        .orderByDesc(AlertRecordEntity::getCreateDate)
                        .last("limit " + size)
        );
        Map<String, String> hostMap = loadHostMap();
        List<AlertRealtimeRsp> result = new ArrayList<>();
        for (AlertRecordEntity entity : list) {
            AlertRealtimeRsp dto = new AlertRealtimeRsp();
            dto.setAlertName(entity.getAlertName());
            dto.setInstance(entity.getInstance());
            dto.setHostName(hostMap.get(normalizeInstance(entity.getInstance())));
            dto.setSeverity(entity.getSeverity());
            dto.setStatus(resolveProblemStatus(entity));
            dto.setTime(entity.getCreateDate());
            dto.setCreateDate(entity.getCreateDate());
            dto.setStartsAt(entity.getStartsAt());
            dto.setEndsAt(entity.getEndsAt());
            dto.setSummary(entity.getSummary());
            dto.setDescription(entity.getDescription());
            dto.setProblem(StrUtil.blankToDefault(entity.getDescription(), entity.getSummary()));
            result.add(dto);
        }
        return result;
    }

    private String resolveProblemStatus(AlertRecordEntity entity) {
        if (entity.getClosed() != null && entity.getClosed() == 1) {
            return "manual";
        }
        if ("resolved".equalsIgnoreCase(entity.getStatus()) || "recover".equalsIgnoreCase(entity.getSeverity())) {
            return "auto";
        }
        return "problem";
    }

    private Map<String, String> loadHostMap() {
        Map<String, String> map = new HashMap<>();
        List<LinuxHostEntity> linuxList = linuxHostMapper.selectList(
                new LambdaQueryWrapper<LinuxHostEntity>().select(LinuxHostEntity::getInstance, LinuxHostEntity::getName)
        );
        for (LinuxHostEntity item : linuxList) {
            putHost(map, item.getInstance(), item.getName());
        }
        List<WindowHostEntity> winList = windowHostMapper.selectList(
                new LambdaQueryWrapper<WindowHostEntity>().select(WindowHostEntity::getInstance, WindowHostEntity::getName)
        );
        for (WindowHostEntity item : winList) {
            putHost(map, item.getInstance(), item.getName());
        }
        List<BusinessSystemEntity> businessList = businessSystemMapper.selectList(
                new LambdaQueryWrapper<BusinessSystemEntity>().select(BusinessSystemEntity::getInstance, BusinessSystemEntity::getName)
        );
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
