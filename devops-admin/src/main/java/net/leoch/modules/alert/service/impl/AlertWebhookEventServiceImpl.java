package net.leoch.modules.alert.service.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import net.leoch.modules.alert.entity.AlertWebhookEventEntity;
import net.leoch.modules.alert.mapper.AlertWebhookEventMapper;
import net.leoch.modules.alert.service.IAlertWebhookEventService;
import org.springframework.stereotype.Service;

@Service
public class AlertWebhookEventServiceImpl extends ServiceImpl<AlertWebhookEventMapper, AlertWebhookEventEntity> implements IAlertWebhookEventService {
}
