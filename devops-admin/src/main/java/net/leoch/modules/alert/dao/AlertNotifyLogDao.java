package net.leoch.modules.alert.dao;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.alert.entity.AlertNotifyLogEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * 告警发送日志
 */
@Mapper
public interface AlertNotifyLogDao extends BaseMapper<AlertNotifyLogEntity> {
}
