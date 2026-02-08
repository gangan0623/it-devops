package net.leoch.modules.oss.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.page.PageData;
import net.leoch.modules.oss.entity.SysOssEntity;
import net.leoch.modules.sys.dto.SysOssPageRequest;

public interface ISysOssService extends IService<SysOssEntity> {
    PageData<SysOssEntity> page(SysOssPageRequest request);
}
