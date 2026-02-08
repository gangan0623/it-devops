package net.leoch.modules.log.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.page.PageData;
import net.leoch.modules.log.vo.rsp.SysLogErrorRsp;
import net.leoch.modules.log.vo.req.SysLogErrorPageReq;
import net.leoch.modules.log.entity.SysLogErrorEntity;

import java.util.List;

public interface ISysLogErrorService extends IService<SysLogErrorEntity> {
    PageData<SysLogErrorRsp> page(SysLogErrorPageReq request);
    List<SysLogErrorRsp> list(SysLogErrorPageReq request);
}
