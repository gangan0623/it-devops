package net.leoch.modules.alert.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.alert.entity.AlertRecordActionEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * 告警记录操作历史
 */
@Mapper
public interface AlertRecordActionMapper extends BaseMapper<AlertRecordActionEntity> {
}
