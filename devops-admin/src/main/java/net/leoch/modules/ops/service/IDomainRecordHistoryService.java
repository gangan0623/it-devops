package net.leoch.modules.ops.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.data.page.PageData;
import net.leoch.modules.ops.entity.DomainRecordHistoryEntity;
import net.leoch.modules.ops.vo.req.DomainRecordHistoryIdReq;
import net.leoch.modules.ops.vo.req.DomainRecordHistoryPageReq;
import net.leoch.modules.ops.vo.rsp.DomainRecordHistoryDetailRsp;
import net.leoch.modules.ops.vo.rsp.DomainRecordHistoryRsp;

public interface IDomainRecordHistoryService extends IService<DomainRecordHistoryEntity> {
    PageData<DomainRecordHistoryRsp> page(DomainRecordHistoryPageReq request);

    DomainRecordHistoryDetailRsp get(DomainRecordHistoryIdReq request);
}
