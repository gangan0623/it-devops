package net.leoch.modules.alert.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.alert.entity.ZabbixAlertEventEntity;
import org.apache.ibatis.annotations.Mapper;

@Mapper
public interface ZabbixAlertEventMapper extends BaseMapper<ZabbixAlertEventEntity> {
}
