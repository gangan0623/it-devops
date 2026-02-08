package net.leoch.modules.alert.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.utils.ConvertUtils;
import net.leoch.modules.alert.mapper.AlertRecordActionMapper;
import net.leoch.modules.alert.dto.AlertRecordActionDTO;
import net.leoch.modules.alert.entity.AlertRecordActionEntity;
import net.leoch.modules.alert.service.AlertRecordActionService;
import net.leoch.modules.sys.mapper.SysUserMapper;
import net.leoch.modules.sys.entity.SysUserEntity;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 告警记录操作历史
 *
 * @author Taohongqiang
 */
@Slf4j
@Service
public class AlertRecordActionServiceImpl extends ServiceImpl<AlertRecordActionMapper, AlertRecordActionEntity> implements AlertRecordActionService {

    private final SysUserDao sysUserDao;

    public AlertRecordActionServiceImpl(SysUserDao sysUserDao) {
        this.sysUserDao = sysUserDao;
    }

    @Override
    public List<AlertRecordActionDTO> listByRecordId(Long recordId) {
        if (recordId == null) {
            return new ArrayList<>();
        }
        List<AlertRecordActionEntity> actions = this.list(
            new LambdaQueryWrapper<AlertRecordActionEntity>()
                .eq(AlertRecordActionEntity::getRecordId, recordId)
                .orderByDesc(AlertRecordActionEntity::getCreateDate)
        );
        List<AlertRecordActionDTO> result = ConvertUtils.sourceToTarget(actions, AlertRecordActionDTO.class);
        List<Long> userIds = result.stream()
            .map(AlertRecordActionDTO::getCreator)
            .filter(Objects::nonNull)
            .distinct()
            .collect(Collectors.toList());
        Map<Long, String> userMap = new HashMap<>();
        if (!userIds.isEmpty()) {
            List<SysUserEntity> users = sysUserDao.selectList(
                new LambdaQueryWrapper<SysUserEntity>()
                    .select(SysUserEntity::getId, SysUserEntity::getUsername)
                    .in(SysUserEntity::getId, userIds)
            );
            for (SysUserEntity user : users) {
                userMap.put(user.getId(), user.getUsername());
            }
        }
        for (AlertRecordActionDTO dto : result) {
            dto.setOperatorName(userMap.get(dto.getCreator()));
        }
        return result;
    }

    @Override
    public void saveAction(Long recordId, String action, String message, String details) {
        AlertRecordActionEntity entity = new AlertRecordActionEntity();
        entity.setRecordId(recordId);
        entity.setAction(action);
        entity.setMessage(message);
        entity.setDetails(details);
        this.save(entity);
    }

    @Override
    public Map<Long, Boolean> loadAckMap(List<Long> recordIds) {
        Map<Long, Boolean> ackMap = new HashMap<>();
        if (recordIds == null || recordIds.isEmpty()) {
            return ackMap;
        }
        List<AlertRecordActionEntity> actions = this.list(
            new LambdaQueryWrapper<AlertRecordActionEntity>()
                .select(AlertRecordActionEntity::getRecordId, AlertRecordActionEntity::getAction)
                .in(AlertRecordActionEntity::getRecordId, recordIds)
        );
        for (AlertRecordActionEntity item : actions) {
            if ("确定".equals(item.getAction())) {
                ackMap.put(item.getRecordId(), true);
            }
        }
        return ackMap;
    }
}
