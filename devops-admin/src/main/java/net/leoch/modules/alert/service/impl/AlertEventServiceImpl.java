package net.leoch.modules.alert.service.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import net.leoch.modules.alert.entity.AlertEventEntity;
import net.leoch.modules.alert.mapper.AlertEventMapper;
import net.leoch.modules.alert.service.IAlertEventService;
import org.springframework.stereotype.Service;

@Service
public class AlertEventServiceImpl extends ServiceImpl<AlertEventMapper, AlertEventEntity> implements IAlertEventService {
}
