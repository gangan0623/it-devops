package net.leoch.modules.log.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.page.PageData;
import net.leoch.modules.log.dto.SysLogErrorDTO;
import net.leoch.modules.log.dto.SysLogErrorPageRequest;
import net.leoch.modules.log.entity.SysLogErrorEntity;

import java.util.List;

public interface SysLogErrorService extends IService<SysLogErrorEntity> {
    PageData<SysLogErrorDTO> page(SysLogErrorPageRequest request);
    List<SysLogErrorDTO> list(SysLogErrorPageRequest request);
}
