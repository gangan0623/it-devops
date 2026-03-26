package net.leoch.modules.ops.mapper;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import net.leoch.modules.ops.entity.DomainRecordHistoryEntity;
import net.leoch.modules.ops.vo.req.DomainRecordHistoryPageReq;
import net.leoch.modules.ops.vo.rsp.DomainRecordHistoryDetailRsp;
import net.leoch.modules.ops.vo.rsp.DomainRecordHistoryRsp;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

/**
 * 域名记录操作历史表
 */
@Mapper
public interface DomainRecordHistoryMapper extends com.baomidou.mybatisplus.core.mapper.BaseMapper<DomainRecordHistoryEntity> {

    /**
     * 域名记录操作历史分页摘要
     */
    IPage<DomainRecordHistoryRsp> selectPageSummary(Page<?> page, @Param("request") DomainRecordHistoryPageReq request);

    /**
     * 域名记录操作历史详情
     */
    DomainRecordHistoryDetailRsp selectDetailById(@Param("id") Long id);
}
