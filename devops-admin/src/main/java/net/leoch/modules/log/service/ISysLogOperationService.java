package net.leoch.modules.log.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.page.PageData;
import net.leoch.modules.log.vo.rsp.SysLogOperationRsp;
import net.leoch.modules.log.vo.req.SysLogOperationPageReq;
import net.leoch.modules.log.entity.SysLogOperationEntity;

import java.util.List;

public interface ISysLogOperationService extends IService<SysLogOperationEntity> {
    PageData<SysLogOperationRsp> page(SysLogOperationPageReq request);
    List<SysLogOperationRsp> list(SysLogOperationPageReq request);
}
