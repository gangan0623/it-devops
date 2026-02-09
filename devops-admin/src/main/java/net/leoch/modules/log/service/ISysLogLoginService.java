package net.leoch.modules.log.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.data.page.PageData;
import net.leoch.modules.log.vo.rsp.SysLogLoginRsp;
import net.leoch.modules.log.vo.req.SysLogLoginPageReq;
import net.leoch.modules.log.entity.SysLogLoginEntity;

import java.util.List;

public interface ISysLogLoginService extends IService<SysLogLoginEntity> {
    PageData<SysLogLoginRsp> page(SysLogLoginPageReq request);
    List<SysLogLoginRsp> list(SysLogLoginPageReq request);
}
