package net.leoch.modules.ops.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.ops.entity.DomainRecordHistoryDetailEntity;
import net.leoch.modules.ops.vo.rsp.DomainRecordHistoryDetailRsp;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 域名记录字段差异明细表
 */
@Mapper
public interface DomainRecordHistoryDetailMapper extends BaseMapper<DomainRecordHistoryDetailEntity> {

    /**
     * 根据历史记录ID查询字段差异明细
     */
    List<DomainRecordHistoryDetailRsp.HistoryFieldDetailRsp> selectByHistoryId(@Param("historyId") Long historyId);
}
