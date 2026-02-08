package net.leoch.modules.log.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.ConvertUtils;
import net.leoch.modules.log.dao.SysLogOperationDao;
import net.leoch.modules.log.dto.SysLogOperationDTO;
import net.leoch.modules.log.dto.SysLogOperationPageRequest;
import net.leoch.modules.log.entity.SysLogOperationEntity;
import net.leoch.modules.log.service.SysLogOperationService;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
public class SysLogOperationServiceImpl extends ServiceImpl<SysLogOperationDao, SysLogOperationEntity> implements SysLogOperationService {

    @Override
    public PageData<SysLogOperationDTO> page(SysLogOperationPageRequest request) {
        IPage<SysLogOperationEntity> page = this.page(request.buildPage(),
            new LambdaQueryWrapper<SysLogOperationEntity>()
                .eq(StrUtil.isNotBlank(request.getStatus()), SysLogOperationEntity::getStatus, request.getStatus())
                .orderByDesc(SysLogOperationEntity::getCreateDate)
        );
        return new PageData<>(ConvertUtils.sourceToTarget(page.getRecords(), SysLogOperationDTO.class), page.getTotal());
    }

    @Override
    public List<SysLogOperationDTO> list(SysLogOperationPageRequest request) {
        List<SysLogOperationEntity> entityList = this.list(
            new LambdaQueryWrapper<SysLogOperationEntity>()
                .eq(request != null && StrUtil.isNotBlank(request.getStatus()), SysLogOperationEntity::getStatus, request != null ? request.getStatus() : null)
                .orderByDesc(SysLogOperationEntity::getCreateDate)
        );
        return ConvertUtils.sourceToTarget(entityList, SysLogOperationDTO.class);
    }
}
