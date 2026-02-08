package net.leoch.modules.log.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.page.PageData;
import net.leoch.modules.log.dto.SysLogOperationDTO;
import net.leoch.modules.log.dto.SysLogOperationPageRequest;
import net.leoch.modules.log.entity.SysLogOperationEntity;

import java.util.List;

public interface ISysLogOperationService extends IService<SysLogOperationEntity> {
    PageData<SysLogOperationDTO> page(SysLogOperationPageRequest request);
    List<SysLogOperationDTO> list(SysLogOperationPageRequest request);
}
