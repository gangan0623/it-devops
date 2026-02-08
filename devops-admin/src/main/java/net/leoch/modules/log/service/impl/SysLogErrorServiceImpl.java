package net.leoch.modules.log.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.ConvertUtils;
import net.leoch.modules.log.mapper.SysLogErrorMapper;
import net.leoch.modules.log.dto.SysLogErrorDTO;
import net.leoch.modules.log.dto.SysLogErrorPageRequest;
import net.leoch.modules.log.entity.SysLogErrorEntity;
import net.leoch.modules.log.service.ISysLogErrorService;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
public class SysLogErrorServiceImpl extends ServiceImpl<SysLogErrorMapper, SysLogErrorEntity> implements ISysLogErrorService {

    @Override
    public PageData<SysLogErrorDTO> page(SysLogErrorPageRequest request) {
        IPage<SysLogErrorEntity> page = this.page(request.buildPage(),
            new LambdaQueryWrapper<SysLogErrorEntity>()
                .orderByDesc(SysLogErrorEntity::getCreateDate)
        );
        return new PageData<>(ConvertUtils.sourceToTarget(page.getRecords(), SysLogErrorDTO.class), page.getTotal());
    }

    @Override
    public List<SysLogErrorDTO> list(SysLogErrorPageRequest request) {
        List<SysLogErrorEntity> entityList = this.list(
            new LambdaQueryWrapper<SysLogErrorEntity>()
                .orderByDesc(SysLogErrorEntity::getCreateDate)
        );
        return ConvertUtils.sourceToTarget(entityList, SysLogErrorDTO.class);
    }
}
