package net.leoch.modules.alert.dao;

import net.leoch.common.dao.BaseDao;
import net.leoch.modules.alert.entity.AlertRecordEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * 告警记录
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-28
 */
@Mapper
public interface AlertRecordDao extends BaseDao<AlertRecordEntity> {
}
