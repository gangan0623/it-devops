package net.leoch.modules.oss.dao;

import net.leoch.common.dao.BaseDao;
import net.leoch.modules.oss.entity.SysOssConfigEntity;
import org.apache.ibatis.annotations.Mapper;

/**
 * 云存储配置
 *
 * @author Taohongqiang
 */
@Mapper
public interface SysOssConfigDao extends BaseDao<SysOssConfigEntity> {
}
