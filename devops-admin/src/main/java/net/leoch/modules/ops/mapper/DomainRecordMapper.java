package net.leoch.modules.ops.mapper;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import net.leoch.modules.ops.entity.DomainRecordEntity;
import net.leoch.modules.ops.vo.req.DomainRecordPageReq;
import net.leoch.modules.ops.vo.rsp.DomainRecordDetailRsp;
import net.leoch.modules.ops.vo.rsp.DomainRecordRsp;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

/**
 * 域名记录表
 */
@Mapper
public interface DomainRecordMapper extends com.baomidou.mybatisplus.core.mapper.BaseMapper<DomainRecordEntity> {

    /**
     * 域名记录分页摘要
     */
    IPage<DomainRecordRsp> selectPageSummary(Page<?> page, @Param("request") DomainRecordPageReq request);

    /**
     * 域名记录详情
     */
    DomainRecordDetailRsp selectDetailById(@Param("id") Long id);
}
