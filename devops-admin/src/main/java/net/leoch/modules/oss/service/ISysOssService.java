package net.leoch.modules.oss.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.page.PageData;
import net.leoch.modules.oss.entity.SysOssEntity;
import net.leoch.modules.sys.vo.req.SysOssPageReq;

public interface ISysOssService extends IService<SysOssEntity> {
    PageData<SysOssEntity> page(SysOssPageReq request);
}
