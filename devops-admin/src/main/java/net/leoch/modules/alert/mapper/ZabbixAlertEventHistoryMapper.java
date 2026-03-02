package net.leoch.modules.alert.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.alert.entity.ZabbixAlertEventHistoryEntity;
import org.apache.ibatis.annotations.Mapper;

@Mapper
public interface ZabbixAlertEventHistoryMapper extends BaseMapper<ZabbixAlertEventHistoryEntity> {
}
