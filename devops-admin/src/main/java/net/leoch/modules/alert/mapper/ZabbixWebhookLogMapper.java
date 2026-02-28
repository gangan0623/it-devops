package net.leoch.modules.alert.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.alert.entity.ZabbixWebhookLogEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * Zabbix webhook 日志
 */
@Mapper
public interface ZabbixWebhookLogMapper extends BaseMapper<ZabbixWebhookLogEntity> {
}

