package net.leoch.modules.ops.service.impl;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import net.leoch.common.data.page.PageData;
import net.leoch.modules.ops.entity.DomainRecordHistoryEntity;
import net.leoch.modules.ops.mapper.DomainRecordHistoryMapper;
import net.leoch.modules.ops.service.IDomainRecordHistoryService;
import net.leoch.modules.ops.vo.req.DomainRecordHistoryIdReq;
import net.leoch.modules.ops.vo.req.DomainRecordHistoryPageReq;
import net.leoch.modules.ops.vo.rsp.DomainRecordHistoryDetailRsp;
import net.leoch.modules.ops.vo.rsp.DomainRecordHistoryRsp;
import org.springframework.stereotype.Service;

@Service
public class DomainRecordHistoryServiceImpl extends ServiceImpl<DomainRecordHistoryMapper, DomainRecordHistoryEntity>
        implements IDomainRecordHistoryService {

    @Override
    public PageData<DomainRecordHistoryRsp> page(DomainRecordHistoryPageReq request) {
        Page<DomainRecordHistoryRsp> page = request.buildPage();
        IPage<DomainRecordHistoryRsp> result = this.baseMapper.selectPageSummary(page, request);
        return new PageData<>(result.getRecords(), result.getTotal());
    }

    @Override
    public DomainRecordHistoryDetailRsp get(DomainRecordHistoryIdReq request) {
        if (request == null || request.getId() == null) {
            return null;
        }
        return this.baseMapper.selectDetailById(request.getId());
    }
}
