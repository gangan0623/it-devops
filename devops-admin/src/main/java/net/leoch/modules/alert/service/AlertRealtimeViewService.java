package net.leoch.modules.alert.service;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.RequiredArgsConstructor;
import net.leoch.modules.alert.entity.AlertRecordEntity;
import net.leoch.modules.alert.mapper.AlertRecordMapper;
import net.leoch.modules.alert.service.impl.AlertRecordDataLoaderService;
import net.leoch.modules.alert.vo.rsp.AlertRealtimeRsp;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
public class AlertRealtimeViewService {
    private final AlertRecordMapper alertRecordMapper;
    private final AlertRecordDataLoaderService alertRecordDataLoaderService;

    public List<AlertRealtimeRsp> recentAlerts(int limit) {
        int size = limit <= 0 ? 10 : Math.min(limit, 50);
        Date now = new Date();
        List<AlertRecordEntity> list = alertRecordMapper.selectList(
                new LambdaQueryWrapper<AlertRecordEntity>()
                        .select(AlertRecordEntity::getId, AlertRecordEntity::getAlertName, AlertRecordEntity::getInstance,
                                AlertRecordEntity::getSeverity, AlertRecordEntity::getStatus, AlertRecordEntity::getStartsAt,
                                AlertRecordEntity::getEndsAt, AlertRecordEntity::getSummary, AlertRecordEntity::getDescription,
                                AlertRecordEntity::getClosed, AlertRecordEntity::getSuppressedUntil, AlertRecordEntity::getLastSeenAt,
                                AlertRecordEntity::getCreateDate)
                        .eq(AlertRecordEntity::getStatus, "firing")
                        .and(w -> w.isNull(AlertRecordEntity::getClosed).or().eq(AlertRecordEntity::getClosed, 0))
                        .and(w -> w.isNull(AlertRecordEntity::getSuppressedUntil).or().lt(AlertRecordEntity::getSuppressedUntil, now))
        );
        list.sort(Comparator
                .comparingInt((AlertRecordEntity item) -> severityWeight(item.getSeverity()))
                .thenComparing((AlertRecordEntity item) -> preferredTime(item), Comparator.nullsLast(Comparator.reverseOrder()))
                .thenComparing(AlertRecordEntity::getStartsAt, Comparator.nullsLast(Comparator.reverseOrder())));
        if (list.size() > size) {
            list = new ArrayList<>(list.subList(0, size));
        }
        var hostMap = alertRecordDataLoaderService.loadHostInfoMap();
        List<AlertRealtimeRsp> result = new ArrayList<>();
        for (AlertRecordEntity entity : list) {
            AlertRealtimeRsp dto = new AlertRealtimeRsp();
            dto.setAlertName(entity.getAlertName());
            dto.setInstance(entity.getInstance());
            dto.setHostName(alertRecordDataLoaderService.resolveHostName(entity.getInstance(), hostMap));
            dto.setSeverity(entity.getSeverity());
            dto.setStatus("problem");
            dto.setTime(preferredTime(entity));
            dto.setCreateDate(preferredTime(entity));
            dto.setStartsAt(entity.getStartsAt());
            dto.setEndsAt(entity.getEndsAt());
            dto.setSummary(entity.getSummary());
            dto.setDescription(entity.getDescription());
            dto.setProblem(StrUtil.blankToDefault(entity.getDescription(), entity.getSummary()));
            result.add(dto);
        }
        return result;
    }

    private int severityWeight(String severity) {
        String value = StrUtil.blankToDefault(severity, "").toLowerCase();
        if ("critical".equals(value)) {
            return 0;
        }
        if ("warning".equals(value)) {
            return 1;
        }
        if ("info".equals(value)) {
            return 2;
        }
        return 3;
    }

    private Date preferredTime(AlertRecordEntity entity) {
        if (entity.getLastSeenAt() != null) {
            return entity.getLastSeenAt();
        }
        if (entity.getStartsAt() != null) {
            return entity.getStartsAt();
        }
        return entity.getCreateDate();
    }
}
