package net.leoch.modules.log.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.page.PageData;
import net.leoch.modules.log.dto.SysLogLoginDTO;
import net.leoch.modules.log.dto.SysLogLoginPageRequest;
import net.leoch.modules.log.entity.SysLogLoginEntity;

import java.util.List;

public interface ISysLogLoginService extends IService<SysLogLoginEntity> {
    PageData<SysLogLoginDTO> page(SysLogLoginPageRequest request);
    List<SysLogLoginDTO> list(SysLogLoginPageRequest request);
}
