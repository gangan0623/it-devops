package net.leoch.modules.alert.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.alert.entity.AlertEventEntity;
import org.apache.ibatis.annotations.Mapper;

@Mapper
public interface AlertEventMapper extends BaseMapper<AlertEventEntity> {
}
