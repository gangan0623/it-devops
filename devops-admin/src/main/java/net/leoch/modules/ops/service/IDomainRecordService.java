package net.leoch.modules.ops.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.data.page.PageData;
import net.leoch.modules.ops.entity.DomainRecordEntity;
import net.leoch.modules.ops.vo.req.DomainRecordDeleteReq;
import net.leoch.modules.ops.vo.req.DomainRecordIdReq;
import net.leoch.modules.ops.vo.req.DomainRecordPageReq;
import net.leoch.modules.ops.vo.req.DomainRecordSaveReq;
import net.leoch.modules.ops.vo.req.DomainRecordUpdateReq;
import net.leoch.modules.ops.vo.rsp.DomainRecordDetailRsp;
import net.leoch.modules.ops.vo.rsp.DomainRecordRsp;

public interface IDomainRecordService extends IService<DomainRecordEntity> {
    PageData<DomainRecordRsp> page(DomainRecordPageReq request);

    DomainRecordDetailRsp get(DomainRecordIdReq request);

    void save(DomainRecordSaveReq request);

    void update(DomainRecordUpdateReq request);

    void delete(DomainRecordDeleteReq request);
}
